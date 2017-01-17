module ManOfStick =
  [<AbstractClass>]
  type private LazyItem<'T>(isInitializer:bool, isValueCreated:bool) =
      member __.IsInitializer  = isInitializer
      member __.IsValueCreated = isValueCreated
      abstract member Value : 'T

  [<AbstractClass>]
  type private LazyInitializer<'T>() =
      inherit LazyItem<'T>(true, false)

  type private LazyValue<'T>(t:'T) =
      inherit LazyItem<'T>(false, true)
      override __.Value = t

  type private LazyException<'T>(e:System.Exception) =
      inherit LazyItem<'T>(false, false)
      override __.Value = raise e

  type private LazyHelper<'T>() =
      static let createInstanceFunc =
          let construct () =
              try
                  System.Activator.CreateInstance typeof<'T> :?> 'T
              with
              | :? System.MissingMethodException ->
                  let nicerException =
#if MORE_REAL
                      System.MissingMemberException(Environment.GetResourceString("Lazy_CreateValue_NoParameterlessCtorForT"))
#else
                      System.MissingMemberException "Lazy_CreateValue_NoParameterlessCtorForT"
#endif
                  raise nicerException

          System.Func<'T> construct

      static member createInstance = createInstanceFunc

  module LazyHelper =
      let getMode isThreadSafe =
          if isThreadSafe then
              System.Threading.LazyThreadSafetyMode.ExecutionAndPublication
          else
              System.Threading.LazyThreadSafetyMode.None

  type Lazzzy<'T> private (valueFactory:System.Func<'T>, mode:System.Threading.LazyThreadSafetyMode, viaConstructor) =
      [<VolatileField>]
      let mutable lazyItem : LazyItem<'T> = Unchecked.defaultof<LazyItem<'T>>

      let none () =
          { new LazyInitializer<'T>() with
              member __.Value =
                  try
                      let value = valueFactory.Invoke ()
                      lazyItem <- LazyValue value
                      value
                  with
                  | ex when not viaConstructor ->
                      lazyItem <- LazyException ex
                      reraise () }

      let executionAndPublication () =
          { new LazyInitializer<'T>() with
              member self.Value =
                  lock self (fun () ->
                      if not lazyItem.IsInitializer then lazyItem.Value
                      else
                          try
                              let value = valueFactory.Invoke ()
                              lazyItem <- LazyValue value
                              value
                          with
                          | ex when not viaConstructor ->
                              lazyItem <- LazyException ex
                              reraise () ) }

      let publicationOnly () =
          { new LazyInitializer<'T>() with
              member self.Value =
                  let value = LazyValue (valueFactory.Invoke ())
                  System.Threading.Interlocked.CompareExchange (&lazyItem, value, self) |> ignore
                  lazyItem.Value }

      do
          match mode with
          | System.Threading.LazyThreadSafetyMode.ExecutionAndPublication -> lazyItem <- executionAndPublication ()
          | System.Threading.LazyThreadSafetyMode.None                    -> lazyItem <- none                    ()
          | System.Threading.LazyThreadSafetyMode.PublicationOnly         -> lazyItem <- publicationOnly         ()
          | _ -> failwith "unknown System.Threading.LazyThreadSafetyMode"

      new ()                          = Lazzzy<'T> (LazyHelper.createInstance, System.Threading.LazyThreadSafetyMode.ExecutionAndPublication, true)
      new (isThreadSafe)              = Lazzzy<'T> (LazyHelper.createInstance, LazyHelper.getMode isThreadSafe,                               true)
      new (valueFactory)              = Lazzzy<'T> (valueFactory,              System.Threading.LazyThreadSafetyMode.ExecutionAndPublication, false)
      new (valueFactory,isThreadSafe) = Lazzzy<'T> (valueFactory,              LazyHelper.getMode isThreadSafe,                               false)
      new (valueFactory, mode)        = Lazzzy<'T> (valueFactory,              mode,                                                          false)
      new (mode)                      = Lazzzy<'T> (LazyHelper.createInstance, mode,                                                          true)

      member __.Value          = lazyItem.Value
      member __.IsValueCreated = lazyItem.IsValueCreated


module PerformanceTests =
  open FSharp.Core.Printf

  open System
  open System.Collections
  open System.Diagnostics
  open System.IO
  open System.Text

  // now () returns current time in milliseconds since start
  let now : unit -> int64 =
    let sw = Stopwatch ()
    sw.Start ()
    fun () -> sw.ElapsedMilliseconds

  // time estimates the time 'action' repeated a number of times
  let time repeat action =
    let inline cc i       = System.GC.CollectionCount i

    let v                 = action ()

    System.GC.Collect (2, System.GCCollectionMode.Forced, true)

    let bcc0, bcc1, bcc2  = cc 0, cc 1, cc 2
    let b                 = now ()

    for i in 1..repeat do
      action () |> ignore

    let e = now ()
    let ecc0, ecc1, ecc2  = cc 0, cc 1, cc 2

    v, (e - b), ecc0 - bcc0, ecc1 - bcc1, ecc2 - bcc2

  let inline dbreak () = System.Diagnostics.Debugger.Break ()

  type TestResult =  TestResult of string*string*string*int64*int*int*int
  let testResult t y x tm cc0 cc1 cc2 = TestResult (t, y, x, tm, cc0, cc1, cc2)
  let testClass (TestResult (t, _, _, _, _, _, _))  = t
  let testY     (TestResult (_, y, _, _, _, _, _))  = y
  let testX     (TestResult (_, _, x, _, _, _, _))  = x

  module NoLazyPerf =
    let createTestCases count ratio =
      let rec simpleLoop v r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let v = i
            simpleLoop v r (s + v) (i + 1)
          else
            simpleLoop v r (s + v) (i + 1)
        else
          s

      let simple () =
        simpleLoop 0 0. 0 0

      simple

  module LazyPerf =
    let inline delay i              = lazy i
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module TrivialFlagPerf =
    type Lazy<'T>(f : unit -> 'T) =
      let mutable hasValue  = false
      let mutable value     = Unchecked.defaultof<'T>

      member x.Value =
        if hasValue then
          value
        else
          value <- f ()
          hasValue <- true
          value

    let inline delay i              = Lazy<_> (fun () -> i)
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module CompactFlagPerf =
    type Lazy<'T>() =
      let mutable value     = null : obj

      member x.UnsafeDelay (f : unit -> 'T) =
        value <- f :> obj

      member x.Value =
        match value with
        | :? 'T as v  -> v
        | _           ->
          let f = value :?> unit -> 'T
          let v = f ()
          value <- box v
          v

    let inline delay i              = let l = Lazy<'T> () in l.UnsafeDelay (fun () -> i); l
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module ExceptionAwareFlagPerf =
    open System.Threading

    type LazyState =
      | HasValue
      | HasException
      | Delayed

    type Lazy<'T>(f : unit -> 'T) =
      let mutable state     = Delayed
      let mutable value     = Unchecked.defaultof<'T>
      let mutable exc       = Unchecked.defaultof<exn>

      member x.Value =
        match state with
        | HasValue      -> value
        | HasException  -> raise exc
        | Delayed       ->
          try
            value <- f ()
            state <- HasValue
            value
          with
          | e ->
            exc <- e
            state <- HasException
            reraise ()

    let inline delay i              = Lazy<_> (fun () -> i)
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module ProtectedFlagPerf =
    open System.Threading

    type Lazy<'T>(f : unit -> 'T) =
      let mutable hasValue  = false
      let mutable value     = Unchecked.defaultof<'T>

      member x.Value =
        let v = value
        if Volatile.Read &hasValue then
          v
        else
          let v = f ()
          value <- v
          Volatile.Write (&hasValue, true)
          v

    let inline delay i              = Lazy<_> (fun () -> i)
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module FullProtectionFlagPerf =
    open System.Threading

    type Lazy<'T>(f : unit -> 'T) =
      let mutable hasValue  = false
      let mutable value     = Unchecked.defaultof<'T>

      member x.Value =
        Monitor.Enter x
        try
          if hasValue then
            value
          else
            value <- f ()
            hasValue <- true
            value
        finally
          Monitor.Exit x

    let inline delay i              = Lazy<_> (fun () -> i)
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module FullProtectionFlag2Perf =
    open System.Threading

    type Lazy<'T>(f : unit -> 'T) =
      let mutable hasValue  = false
      let mutable value     = Unchecked.defaultof<'T>

      member x.Value =
        let v = value
        if Volatile.Read &hasValue then
          value
        else
          Monitor.Enter x
          try
            if Volatile.Read &hasValue then
              value
            else
              value <- f ()
              Volatile.Write (&hasValue, true)
              value
          finally
            Monitor.Exit x

    let inline delay i              = Lazy<_> (fun () -> i)
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module LazyProtectedExecutionAndPublicationPerf =
    open System.Threading

    let inline delay i              = Lazy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.ExecutionAndPublication)
    let inline value (l : Lazy<_>)  = l.Value

    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module LazyProtectedPublicationPerf =
    open System.Threading

    let inline delay i              = Lazy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.PublicationOnly)
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module LazyNoProtectionPerf =
    open System.Threading

    let inline delay i              = Lazy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.None)
    let inline value (l : Lazy<_>)  = l.Value


    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module LazzzyProtectedExecutionAndPublicationPerf =
    open ManOfStick
    open System.Threading

    let inline delay i                = Lazzzy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.ExecutionAndPublication)
    let inline value (l : Lazzzy<_>)  = l.Value

    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module LazzzyProtectedPublicationPerf =
    open ManOfStick
    open System.Threading

    let inline delay i                = Lazzzy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.PublicationOnly)
    let inline value (l : Lazzzy<_>)  = l.Value

    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module LazzzyNoProtectionPerf =
    open ManOfStick
    open System.Threading

    let inline delay i                = Lazzzy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.None)
    let inline value (l : Lazzzy<_>)  = l.Value

    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple


  module NewLazyProtectedExecutionAndPublicationPerf =
    open NewLazy
    open System.Threading

    let inline delay i                = Lazy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.ExecutionAndPublication)
    let inline value (l : Lazy<_>)    = l.Value

    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module NewLazyProtectedPublicationPerf =
    open NewLazy
    open System.Threading

    let inline delay i                = Lazy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.PublicationOnly)
    let inline value (l : Lazy<_>)    = l.Value

    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple

  module NewLazyNoProtectionPerf =
    open NewLazy
    open System.Threading

    let inline delay i                = Lazy<_> (Func<_> (fun () -> i), LazyThreadSafetyMode.None)
    let inline value (l : Lazy<_>)    = l.Value

    let createTestCases count ratio =
      let rec simpleLoop l r s i =
        if i < count then
          let r = r + ratio
          if r >= 1. then
            let r = r - 1.
            let l = delay i
            simpleLoop l r (s + value l) (i + 1)
          else
            simpleLoop l r (s + value l) (i + 1)
        else
          s

      let simple () =
        simpleLoop (delay 0) 0. 0 0

      simple


  let run () =
#if DEBUG
    let count   = 100
#else
    let count   = 10000000
#endif
    let ratios  =
      [|
#if DEBUG
        0.33
#else
        0.0
        0.2
        0.4
        0.6
        0.8
        1.0
#endif
      |]

    let testCases =
      [|
        "no lazy"                           , NoLazyPerf.createTestCases
        "lazy"                              , LazyPerf.createTestCases
        "Lazy (Execution & Publication)"    , LazyProtectedExecutionAndPublicationPerf.createTestCases
        "Lazy (Publication)"                , LazyProtectedPublicationPerf.createTestCases
        "Lazy (None)"                       , LazyNoProtectionPerf.createTestCases
        "Lazzzy (Execution & Publication)"  , LazzzyProtectedExecutionAndPublicationPerf.createTestCases
        "Lazzzy (Publication)"              , LazzzyProtectedPublicationPerf.createTestCases
        "Lazzzy (None)"                     , LazzzyNoProtectionPerf.createTestCases
        "NewLazy (Execution & Publication)" , NewLazyProtectedExecutionAndPublicationPerf.createTestCases
        "NewLazy (Publication)"             , NewLazyProtectedPublicationPerf.createTestCases
        "NewLazy (None)"                    , NewLazyNoProtectionPerf.createTestCases
        "Flag (Trivial)"                    , TrivialFlagPerf.createTestCases
        "Flag (Compact)"                    , CompactFlagPerf.createTestCases
        "Flag (Exception aware)"            , ExceptionAwareFlagPerf.createTestCases
        "Flag (Protected)"                  , ProtectedFlagPerf.createTestCases
        "Flag (Full protection)"            , FullProtectionFlagPerf.createTestCases
        "Flag (Full protection w. DC)"      , FullProtectionFlag2Perf.createTestCases
      |]
    let results = ResizeArray 16

    for ratio in ratios do

      printfn "Running test cases with ratio=%f" ratio

      let x = sprintf "%d%%" (ratio*100. |> round |> int)

      for name, creator in testCases do
        printfn "  Running test case %A" name

        let result t a =
          let v, time, cc0, cc1, cc2 = time 1 a
          results.Add <| testResult t name x time cc0 cc1 cc2
          printfn "    = %A" v

        let simple = creator count ratio

        result "Simple" simple

    let results = results.ToArray ()

    let testXs      = results |> Array.groupBy testX |> Array.map fst
    let testYs      = results |> Array.groupBy testY
    let testClasses = results |> Array.groupBy testClass

    let header  = "Name" + (testXs |> Array.map (fun i -> ",'" + string i) |> Array.reduce (+))

    for name, results in testClasses do
      use perf      = new StreamWriter ("perf_" + name + ".csv")
      use cc        = new StreamWriter ("cc_"   + name + ".csv")
      let line sw l = (sw : StreamWriter).WriteLine (l : string)
      let linef sw f= kprintf (line sw) f

      line perf header
      line cc   header

      for name, result in testYs do
        let write sb s  = (sb : StringBuilder).Append (s : string) |> ignore
        let field sb s  = (sb : StringBuilder).Append ',' |> ignore; write sb s
        let fieldf sb f = kprintf (field sb) f
        let psb         = StringBuilder 16
        let csb         = StringBuilder 16
        write psb name
        write csb name
        let m = result |> Array.map (fun tc -> testX tc, tc) |> Map.ofArray
        for testInner in testXs do
          let (TestResult (_, _, _, tm, cc0 ,_, _)) = m.[testInner]
          fieldf psb "%d" tm
          fieldf csb "%d" cc0

        line perf <| psb.ToString ()
        line cc   <| csb.ToString ()

open System

[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
    PerformanceTests.run ()
    0
  with
  | e ->
    printfn "Caught: %s" e.Message
    999
