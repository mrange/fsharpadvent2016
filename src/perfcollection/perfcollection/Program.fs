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

  type TestResult =  TestResult of string*string*int*int64*int*int*int
  let testResult t tc i tm cc0 cc1 cc2 = TestResult (t, tc, i, tm, cc0, cc1, cc2)
  let testCase  (TestResult (_, tc, _, _, _, _, _)) = tc
  let testClass (TestResult (t, _, _, _, _, _, _))  = t
  let testInner (TestResult (_, _, i, _, _, _, _))  = i

  module ImperativePerf =
    let createTestCases count =
      let rec simpleLoop sum i =
        if i < count then
          let n = int64 i
          let n = n*5L
          if n % 7L <> 0L then
            let n = n / 11L
            simpleLoop (sum + n) (i + 1)
          else
            simpleLoop sum (i + 1)
        else
          sum
      let simple () =
        simpleLoop 0L 0
      simple

  module SeqPerf =
    let createTestCases count =
      let inline range  c   = Seq.init c id
      let inline filter f s = Seq.filter f s
      let inline map    m s = Seq.map    m s
      let inline sum      s = Seq.sum    s
      let simple () =
        range count
        |> map    (fun n -> int64 n * 5L)
        |> filter (fun n -> n % 7L <> 0L)
        |> map    (fun n -> n / 11L)
        |> sum
      simple

  module LinqPerf =
    open System.Linq

    let createTestCases count =
      let inline range  c   = Enumerable.Range (0, c)
      let inline filter f s = Enumerable.Where (s, Func<_, _> f)
      let inline map    m s = Enumerable.Select(s, Func<_, _> m)
      let inline sum      s = Enumerable.Sum   (s : seq<int64>)
      let simple () =
        range count
        |> map    (fun n -> int64 n * 5L)
        |> filter (fun n -> n % 7L <> 0L)
        |> map    (fun n -> n / 11L)
        |> sum
      simple

  module NessosStreamsPerf =
    open Nessos.Streams

    let createTestCases count =
      let inline range  c   = Stream.initInfinite id |> Stream.take c
      let inline filter f s = Stream.filter f s
      let inline map    m s = Stream.map    m s
      let inline sum      s = Stream.sum    s
      let simple () =
        range count
        |> map    (fun n -> int64 n * 5L)
        |> filter (fun n -> n % 7L <> 0L)
        |> map    (fun n -> n / 11L)
        |> sum
      simple

  module NessosLinqOptimizerPerf =
    open Nessos.LinqOptimizer.FSharp

    let createTestCases count =
      let simple () =
        Query.range (0, count)
        |> Query.map    (fun n -> int64 n * 5L)
        |> Query.filter (fun n -> n % 7L <> 0L)
        |> Query.map    (fun n -> n / 11L)
        |> Query.fold   (fun s v -> s + v) 0L
        |> Query.run
      simple

  module SeqComposerPerf =
    open SeqComposer.Microsoft.FSharp.Collections

    let createTestCases count =
      let inline range  c   = Seq.init c id
      let inline filter f s = Seq.filter f s
      let inline map    m s = Seq.map    m s
      let inline sum      s = Seq.sum    s
      let simple () =
        range count
        |> map    (fun n -> int64 n * 5L)
        |> filter (fun n -> n % 7L <> 0L)
        |> map    (fun n -> n / 11L)
        |> sum
      simple

  module PullStreamPerf =
    open PullStream

    let createTestCases count =
      let inline range  c   = Stream.range 0 (c - 1)
      let inline filter f s = Stream.filter f s
      let inline map    m s = Stream.map    m s
      let inline sum      s = Stream.sum    s
      let simple () =
        range count
        |> map    (fun n -> int64 n * 5L)
        |> filter (fun n -> n % 7L <> 0L)
        |> map    (fun n -> n / 11L)
        |> sum
      simple

  module PushStreamPerf =
    open PushStream

    let createTestCases count =
      let inline range  c   = Stream.range 0 1 (c - 1)
      let inline filter f s = Stream.filter f s
      let inline map    m s = Stream.map    m s
      let inline sum      s = Stream.sum    s
      let simple () =
        range count
        |> map    (fun n -> int64 n * 5L)
        |> filter (fun n -> n % 7L <> 0L)
        |> map    (fun n -> n / 11L)
        |> sum
      simple

  module PushPipePerf =
    open PushPipe

    let createTestCases count =
      let range             = Pipe.acceptRange
      let inline filter f s = Pipe.filter f s
      let inline map    m s = Pipe.map    m s
      let inline sum      s = Pipe.sum    s
      let simplePipe =
        range
        |> map    (fun n -> int64 n * 5L)
        |> filter (fun n -> n % 7L <> 0L)
        |> map    (fun n -> n / 11L)
        |> sum
      let simpleRange = 0, 1, count - 1
      let simple () =
        Pipe.reset    simplePipe
        Pipe.receive  simplePipe simpleRange |> ignore
        Pipe.finalize simplePipe
      simple

  let run () =
//    let random      = makeRandom 19740531
    let total       = 10000000
    let outers      =
      [|
#if DEBUG
        1000
#else
        10000000
        1000000
        100000
        10000
        1000
        100
        10
        1
#endif
      |]

    let testCases =
      [|
        "Imperative"                    , ImperativePerf.createTestCases
        "Seq"                           , SeqPerf.createTestCases
        "Linq"                          , LinqPerf.createTestCases
        "Nessos.Streams"                , NessosStreamsPerf.createTestCases
//        "Nessos.LinqOptimizer"          , NessosLinqOptimizerPerf.createTestCases
        "SeqComposer"                   , SeqComposerPerf.createTestCases
        "PullStream"                    , PullStreamPerf.createTestCases
        "PushStream"                    , PushStreamPerf.createTestCases
        "PushPipe"                      , PushPipePerf.createTestCases
      |]

    let results = ResizeArray 16

    for outer in outers do
      let inner       = total / outer

      printfn "Running test cases with outer=%d and inner=%d" outer inner

      for name, creator in testCases do
        printfn "  Running test case %A" name

        let result t a =
          let v, time, cc0, cc1, cc2 = time outer a
          results.Add <| testResult t name inner time cc0 cc1 cc2
          printfn "    = %A" v

        let simple = creator inner

        result "Simple" simple

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
