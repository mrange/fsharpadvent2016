module PerformanceTests =
  open FSharp.Core.Printf

  open System
  open System.Collections
  open System.Diagnostics
  open System.IO
  open System.Text

  // now () returns current time in milliseconds since start
  let now : unit -> int64 =
    let sw = System.Diagnostics.Stopwatch ()
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

  let makeRandom (seed : int) =
    let mutable state = int64 seed
    let m = 0x7FFFFFFFL // 2^31 - 1
    let d = 1. / float m
    let a = 48271L      // MINSTD
    let c = 0L
    fun (b : int) (e : int) ->
      state <- (a*state + c) % m
      let r = float state * d
      let v = float (e - b)*r + float b |> int
      v

  let shuffle random vs =
    let a = Array.copy vs
    for i in 0..(vs.Length - 2) do
      let s =  random i vs.Length
      let t =  a.[s]
      a.[s] <- a.[i]
      a.[i] <- t
    a

  type Checker () =
    [<Conditional ("DEBUG")>]
    static member inline check fb str =
      if not (fb ()) then
        printfn "Check failed: %s" str
        failwith str

  // Key is reference type in order to not kill performance in collections that always boxes
  //  the key/value
  type Key(v : int) =
    member x.Value = v

    interface IComparable with
      member x.CompareTo(o : obj) =
        match o with
        | :? Key as k -> v.CompareTo (k.Value)
        | _           -> -1

    interface IComparable<Key> with
      member x.CompareTo(o : Key) = v.CompareTo (o.Value)

    interface IEquatable<Key> with
      member x.Equals(o : Key)  = v = o.Value

    override x.Equals(o : obj)  =
      match o with
      | :? Key as k -> v = k.Value
      | _           -> false
    override x.GetHashCode()    = v.GetHashCode ()
    override x.ToString()       = sprintf "%d" v
  let makeKey i = Key i

  type TestResult =  TestResult of string*string*int*int64*int*int*int
  let testResult t tc i tm cc0 cc1 cc2 = TestResult (t, tc, i, tm, cc0, cc1, cc2)
  let testCase  (TestResult (_, tc, _, _, _, _, _)) = tc
  let testClass (TestResult (t, _, _, _, _, _, _))  = t
  let testInner (TestResult (_, _, i, _, _, _, _))  = i

//  type Key = int
//  let makeKey i : int = i

  let makeValue i = string i

  module CsPersistentHashMap =
    open PHM.CS

    let createTestCases lookups inserts removals = 
      let inline containsKey  k   (phm : PersistentHashMap<_, _>) = 
        let r, _ = phm.TryFind k
        r
      let inline isEmpty          (phm : PersistentHashMap<_, _>) = phm.IsEmpty
      let length (phm : PersistentHashMap<_, _>) =
        let mutable l = 0
        let visitor _ _ = l <- l + 1; true
        phm.Visit (Func<_, _, _> visitor) |> ignore
        l
      let inline set          k v (phm : PersistentHashMap<_, _>) = phm.Set (k, v)
      let inline unset        k   (phm : PersistentHashMap<_, _>) = phm.Unset k

      let inline doInsert phm =
        let rec loop phm i =
          if i < Array.length inserts then
            let k, v = inserts.[i]
            loop (set k v phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doRemove phm =
        let rec loop phm i =
          if i < Array.length removals then
            let k, _ = removals.[i]
            loop (unset k phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doLookup phm =
        let rec loop i =
          if i < Array.length lookups then
            let k, _ = lookups.[i]
            containsKey k phm && loop (i + 1)
          else
            true
        loop 0

      let empty     = PersistentHashMap.Empty<_, _> ()

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module FsPersistentHashMap =
    open Persistent

    let createTestCases lookups inserts removals = 
      let inline containsKey  k   phm = PersistentHashMap.containsKey k   phm
      let inline length           phm = PersistentHashMap.length          phm
      let inline isEmpty          phm = PersistentHashMap.isEmpty         phm
      let inline set          k v phm = PersistentHashMap.set         k v phm
      let inline unset        k   phm = PersistentHashMap.unset       k   phm

      let inline doInsert phm =
        let rec loop phm i =
          if i < Array.length inserts then
            let k, v = inserts.[i]
            loop (set k v phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doRemove phm =
        let rec loop phm i =
          if i < Array.length removals then
            let k, _ = removals.[i]
            loop (unset k phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doLookup phm =
        let rec loop i =
          if i < Array.length lookups then
            let k, _ = lookups.[i]
            containsKey k phm && loop (i + 1)
          else
            true
        loop 0

      let empty     = PersistentHashMap.empty

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module FSharpx =
    open Patches.FSharpx.Collections

    let createTestCases lookups inserts removals = 
      let inline containsKey  k   phm = PersistentHashMap.containsKey k   phm
      let inline length           phm = PersistentHashMap.length          phm
      let inline isEmpty          phm = PersistentHashMap.length          phm = 0
      let inline set          k v phm = PersistentHashMap.add         k v phm
      let inline unset        k   phm = PersistentHashMap.remove      k   phm

      let inline doInsert phm =
        let rec loop phm i =
          if i < Array.length inserts then
            let k, v = inserts.[i]
            loop (set k v phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doRemove phm =
        let rec loop phm i =
          if i < Array.length removals then
            let k, _ = removals.[i]
            loop (unset k phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doLookup phm =
        let rec loop i =
          if i < Array.length lookups then
            let k, _ = lookups.[i]
            containsKey k phm && loop (i + 1)
          else
            true
        loop 0

      let empty     = PersistentHashMap.empty

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module FsMap =
    let createTestCases lookups inserts removals = 
      let inline containsKey  k   m = Map.containsKey k   m
      let inline length           m = m |> Map.toArray |> Array.length
      let inline isEmpty          m = Map.isEmpty         m
      let inline set          k v m = Map.add         k v m
      let inline unset        k   m = Map.remove      k   m

      let inline doInsert phm =
        let rec loop phm i =
          if i < Array.length inserts then
            let k, v = inserts.[i]
            loop (set k v phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doRemove phm =
        let rec loop phm i =
          if i < Array.length removals then
            let k, _ = removals.[i]
            loop (unset k phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doLookup phm =
        let rec loop i =
          if i < Array.length lookups then
            let k, _ = lookups.[i]
            containsKey k phm && loop (i + 1)
          else
            true
        loop 0

      let empty     = Map.empty

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module PrimeVmap =
    open Prime

    let createTestCases lookups inserts removals = 
      let inline containsKey  k   m = HMap.containsKey k   m
      let inline length           m = HMap.toSeq           m |> Seq.length
      let inline isEmpty          m = HMap.isEmpty         m
      let inline set          k v m = HMap.add         k v m
      let inline unset        k   m = HMap.remove      k   m

      let inline doInsert phm =
        let rec loop phm i =
          if i < Array.length inserts then
            let k, v = inserts.[i]
            loop (set k v phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doRemove phm =
        let rec loop phm i =
          if i < Array.length removals then
            let k, _ = removals.[i]
            loop (unset k phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doLookup phm =
        let rec loop i =
          if i < Array.length lookups then
            let k, _ = lookups.[i]
            containsKey k phm && loop (i + 1)
          else
            true
        loop 0

      let empty     = HMap.makeEmpty ()

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module ImmsImmMap =
    open Imms

    let createTestCases lookups inserts removals = 
      let inline containsKey  k   m = ImmMap.containsKey k   m
      let inline length           m = ImmMap.length          m
      let inline isEmpty          m = ImmMap.isEmpty         m
      let inline set          k v m = ImmMap.set         k v m
      let inline unset        k   m = ImmMap.remove      k   m

      let inline doInsert phm =
        let rec loop phm i =
          if i < Array.length inserts then
            let k, v = inserts.[i]
            loop (set k v phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doRemove phm =
        let rec loop phm i =
          if i < Array.length removals then
            let k, _ = removals.[i]
            loop (unset k phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doLookup phm =
        let rec loop i =
          if i < Array.length lookups then
            let k, _ = lookups.[i]
            containsKey k phm && loop (i + 1)
          else
            true
        loop 0

      let empty     = ImmMap.empty

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module SCI =
    open System.Collections.Immutable

    let createTestCases lookups inserts removals = 
      let inline containsKey  k   (m : ImmutableDictionary<_, _>) = m.ContainsKey k
      let inline length           (m : ImmutableDictionary<_, _>) = m.Count
      let inline isEmpty          (m : ImmutableDictionary<_, _>) = m.IsEmpty
      let inline set          k v (m : ImmutableDictionary<_, _>) = m.SetItem (k, v)
      let inline unset        k   (m : ImmutableDictionary<_, _>) = m.Remove k

      let inline doInsert phm =
        let rec loop phm i =
          if i < Array.length inserts then
            let k, v = inserts.[i]
            loop (set k v phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doRemove phm =
        let rec loop phm i =
          if i < Array.length removals then
            let k, _ = removals.[i]
            loop (unset k phm) (i + 1)
          else
            phm
        loop phm 0

      let inline doLookup phm =
        let rec loop i =
          if i < Array.length lookups then
            let k, _ = lookups.[i]
            containsKey k phm && loop (i + 1)
          else
            true
        loop 0

      let empty     = ImmutableDictionary<_, _>.Empty

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  let run () =
    let random      = makeRandom 19740531
    let multiplier  = 4 // controls the ratio of duplicates in test data
    let total       = 400000
    let outers      =
      [|
#if DEBUG
        1000
#else
//        400000
        40000
        4000
        400
        40
        4
#endif
      |]

    let testCases =
      [|
        "Persistent Hash Map (F#)"      , FsPersistentHashMap.createTestCases
        "FSharpx.Collections"           , FSharpx.createTestCases
        "Prime.HMap"                    , PrimeVmap.createTestCases
        "Imms.ImmMap"                   , ImmsImmMap.createTestCases
        "System.Collections.Immutable"  , SCI.createTestCases
        "Map (F#)"                      , FsMap.createTestCases
      |]

    let results = ResizeArray 16

    for outer in outers do
      let inner       = total / outer

      printfn "Running test cases with outer=%d and inner=%d" outer inner

      let inserts     =
        [|
          for i in 0..(inner - 1) -> random 0 (inner*multiplier) |> makeKey, makeValue i
        |]
      let removals    = shuffle random inserts
      let lookups     = shuffle random inserts

      for name, creator in testCases do
        printfn "  Running test case %A" name

        let result t a =
          let _, time, cc0, cc1, cc2 = time outer a
          results.Add <| testResult t name inner time cc0 cc1 cc2

        let lookup, insert, remove = creator lookups inserts removals

        result "Lookup" lookup
        result "Insert" insert
        result "Remove" remove

    let results = results.ToArray ()

    let testInners  = results |> Array.groupBy testInner |> Array.map fst
    let testClasses = results |> Array.groupBy testClass

    let header      = "Name" + (testInners |> Array.map (fun i -> ",'" + string i) |> Array.reduce (+))

    for name, results in testClasses do
      use perf      = new StreamWriter ("perf_" + name + ".csv")
      use cc        = new StreamWriter ("cc_"   + name + ".csv")
      let line sw l = (sw : StreamWriter).WriteLine (l : string) 
      let linef sw f= kprintf (line sw) f
      let testCases = results |> Array.groupBy testCase

      line perf header
      line cc   header

      for name, result in testCases do
        let write sb s  = (sb : StringBuilder).Append (s : string) |> ignore
        let field sb s  = (sb : StringBuilder).Append ',' |> ignore; write sb s
        let fieldf sb f = kprintf (field sb) f
        let psb         = StringBuilder 16
        let csb         = StringBuilder 16
        write psb name
        write csb name
        let m = result |> Array.map (fun tc -> testInner tc, tc) |> Map.ofArray
        for testInner in testInners do
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
