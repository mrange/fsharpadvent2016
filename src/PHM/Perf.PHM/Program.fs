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
        inserts
        |> Array.fold (fun s (k, v) -> set k v s) phm

      let doRemove phm =
        removals
        |> Array.fold (fun s (k, v) -> unset k s) phm

      let inline doLookup fa phm =
        fa
        |> Array.forall (fun (k, _) -> containsKey k phm)

      let empty     = PersistentHashMap.Empty<_, _> ()

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup lookups inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module FsPersistentHashMap =
    open Persistent

    let createTestCases lookups inserts removals = 
      let inline doInsert phm =
        inserts
        |> Array.fold (fun s (k, v) -> PersistentHashMap.set k v s) phm

      let doRemove phm =
        removals
        |> Array.fold (fun s (k, v) -> PersistentHashMap.unset k s) phm

      let inline doLookup fa phm =
        fa
        |> Array.forall (fun (k, _) -> PersistentHashMap.containsKey k phm)

      let empty     = PersistentHashMap.empty

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> PersistentHashMap.length result = PersistentHashMap.length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> PersistentHashMap.isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup lookups inserted
        Checker.check (fun () -> result) "Expected true for all"

      lookup, insert, remove

  module FsMap =
    let length m =
      m |> Map.toArray |> Array.length

    let createTestCases lookups inserts removals = 
      let inline doInsert phm =
        inserts
        |> Array.fold (fun s (k, v) -> Map.add k v s) phm

      let doRemove phm =
        removals
        |> Array.fold (fun s (k, v) -> Map.remove k s) phm

      let inline doLookup fa phm =
        fa
        |> Array.forall (fun (k, _) -> Map.containsKey k phm)

      let empty     = Map.empty

      let inserted  = doInsert empty

      let insert () =
        let result    = doInsert empty
        Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

      let remove () =
        let result    = doRemove inserted
        Checker.check (fun () -> Map.isEmpty result) "Expected to be empty"

      let lookup () =
        let result    = doLookup lookups inserted
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
//        4
        40000
        400
        4000
        40
//        400000
#endif
      |]

    let testCases =
      [|
        "Persistent Hash Map (C#)"      , CsPersistentHashMap.createTestCases
        "Persistent Hash Map (F#)"      , FsPersistentHashMap.createTestCases
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
