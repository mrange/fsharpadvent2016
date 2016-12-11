// ----------------------------------------------------------------------------------------------
// Copyright 2016 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

open Common

module FsCheckConfig =
  open FsCheck
#if DEBUG
  let testCount = 100
#else
  let testCount = 1000
#endif
  let config = { Config.Quick with MaxTest = testCount; MaxFail = testCount }

module FsPropertyTests =
  open Persistent
  open System

  module FsLinq =
    open System.Linq

    let inline first    source                        = Enumerable.First    (source)
    let inline groupBy  (selector : 'T -> 'U) source  = Enumerable.GroupBy  (source, Func<'T, 'U> selector)
    let inline last     source                        = Enumerable.Last     (source)
    let inline map      (selector : 'T -> 'U) source  = Enumerable.Select   (source, Func<'T, 'U> selector)
    let inline sortBy   (selector : 'T -> 'U) source  = Enumerable.OrderBy  (source, Func<'T, 'U> selector)
    let inline toArray  source                        = Enumerable.ToArray  (source)

  let uniqueKey vs =
    vs
    |> FsLinq.groupBy fst
    |> FsLinq.map (fun g -> g.Key, (g |> FsLinq.map snd |> FsLinq.last))
    |> FsLinq.sortBy fst
    |> FsLinq.toArray

  let fromArray kvs =
    Array.fold
      (fun s (k, v) -> PersistentHashMap.set k v s)
      PersistentHashMap.empty
      kvs

  let toArray phm =
    phm
    |> PersistentHashMap.toArray

  let toSortedKeyArray phm =
    let vs = phm |> toArray
    vs |> Array.sortInPlaceBy fst
    vs

  let notIdentical<'T when 'T : not struct> (f : 'T) (s : 'T) = obj.ReferenceEquals (f, s) |> not

  type ComplexType =
    | IntKey    of  int
    | StringKey of  int
    | TupleKey  of  int*string

  type HalfHash(v : int) =
    member x.Value = v

    interface IComparable<HalfHash> with
      member x.CompareTo(o : HalfHash)  = v.CompareTo o.Value

    interface IEquatable<HalfHash> with
      member x.Equals(o : HalfHash)  = v = o.Value

    override x.Equals(o : obj)  =
      match o with
      | :? HalfHash as k -> v = k.Value
      | _                -> false
    override x.GetHashCode()    = (v.GetHashCode ()) >>> 16 // In order to get a fair bunch of duplicated hashes
    override x.ToString()       = sprintf "%d" v

  type Action =
    | Add     of int*string
    | Remove  of int

  let checkInvariant (phm : PersistentHashMap<_, _>) = phm.CheckInvariant ()

  type Properties () =

    static member ``PHM toArray must contain all added values`` (vs : (int*string) []) =
      let expected  = uniqueKey vs
      let phm       = vs |> fromArray
      let actual    = phm |> toSortedKeyArray

      notIdentical expected actual
      && checkInvariant phm
      && expected = actual

    static member ``PHM TryFind must return all added values`` (vs : (ComplexType*ComplexType) []) =
      let unique    = uniqueKey vs
      let phm       = unique |> fromArray

      let rec loop i =
        if i < unique.Length then
          let k, v = unique.[i]
          match PersistentHashMap.tryFind k phm with
          | Some fv when fv = v -> loop (i + 1)
          | _                   -> false
        else
          true

      checkInvariant phm
      && loop 0

    static member ``PHM Unset on all added values must yield empty map`` (vs : (HalfHash*int) []) =
      let unique    = uniqueKey vs
      let phm       = unique |> fromArray

      let rec loop (phm : PersistentHashMap<_, _>) i =
        if checkInvariant phm |> not then
          None
        elif i < unique.Length then
          if phm |> PersistentHashMap.isEmpty then
            None
          else
            let k, v = unique.[i]
            loop (PersistentHashMap.unset k phm) (i + 1)
        else
          Some phm

      match loop phm 0 with
      | Some phm  -> PersistentHashMap.isEmpty phm
      | None      -> false

    static member ``PHM should behave as Map`` (vs : Action []) =
      let compare map (phm : PersistentHashMap<_, _>) =
        let empty =
          match map |> Map.isEmpty, phm |> PersistentHashMap.isEmpty with
          | true  , true
          | false , false -> true
          | _     , _     -> false

        let visitor k v =
          match map |> Map.tryFind k with
          | Some fv -> v = fv
          | _       -> false

        checkInvariant phm
        && (PersistentHashMap.length phm = map.Count)
        && empty
        && PersistentHashMap.visit visitor phm

      let ra = ResizeArray<int> ()

      let rec loop map (phm : PersistentHashMap<_, _>) i =
        if i < vs.Length then
          match vs.[i] with
          | Add (k, v)  ->
            ra.Add k
            let map = map |> Map.add k v
            let phm = PersistentHashMap.set k v phm
            compare map phm && loop map phm (i + 1)
          | Remove r    ->
            if ra.Count > 0 then
              let r   = abs r % ra.Count
              let k   = ra.[r]
              ra.RemoveAt r
              let map = map |> Map.remove k
              let phm = PersistentHashMap.unset k phm
              compare map phm && loop map phm (i + 1)
            else
              loop map phm (i + 1)
        else
          true

      loop Map.empty PersistentHashMap.empty 0

  open FsCheck

  let testLongInsert () =
#if DEBUG
    let count       = 1000
#else
    let count       = 1000000
#endif
    let multiplier  = 8
    printfn "testLongInsert: count:%d, multiplier:%d" count multiplier
    let random      = makeRandom 19740531
    let inserts     = [| for x in 1..count -> random 0 (count * multiplier) |]
    let removals    = shuffle random inserts

    let mutable phm = PersistentHashMap.empty

    for i in inserts do
      phm <- phm |> PersistentHashMap.set i i
      match phm |> PersistentHashMap.tryFind i with
      | Some v when v = i -> ()
      | _                 -> failwith "testLongInsert/insert/tryFind failed"

#if DEBUG
      if phm |> checkInvariant |> not then
        failwith "testLongInsert/insert/checkInvariant failed"
#endif

    for r in removals do
      phm <- phm |> PersistentHashMap.unset r
      match phm |> PersistentHashMap.tryFind r with
      | None  -> ()
      | _     -> failwith "testLongInsert/remove/tryFind failed"

#if DEBUG
      if phm |> checkInvariant |> not then
        failwith "testLongInsert/remove/checkInvariant failed"
#endif

    printfn "  Done"

  let run () =
    // Properties.``PHM TryFind must return all added values`` [|(IntKey 33, StringKey 0); (StringKey -31, TupleKey (0,""))|] |> printfn "%A"
    Check.All<Properties> FsCheckConfig.config
    testLongInsert ()

module PropertyTests =
  open FsCheck
  open PHM.CS

  open System
  open System.Collections.Generic

  [<AllowNullLiteral>]
  type Empty () =
    inherit obj ()

  module FsLinq =
    open System.Linq

    let inline first    source                        = Enumerable.First    (source)
    let inline groupBy  (selector : 'T -> 'U) source  = Enumerable.GroupBy  (source, Func<'T, 'U> selector)
    let inline last     source                        = Enumerable.Last     (source)
    let inline map      (selector : 'T -> 'U) source  = Enumerable.Select   (source, Func<'T, 'U> selector)
    let inline sortBy   (selector : 'T -> 'U) source  = Enumerable.OrderBy  (source, Func<'T, 'U> selector)
    let inline toArray  source                        = Enumerable.ToArray  (source)

  module Common =
    let notIdentical<'T when 'T : not struct> (f : 'T) (s : 'T) = obj.ReferenceEquals (f, s) |> not

    let check b str =
      if not b then
        printfn "Check failed: %s" str
        failwith str

    let popCount v =
      let rec loop c v =
        if v <> 0u then
          loop (c + 1) (v &&& (v - 1u))
        else
          c
      loop 0 v

    let copyArrayMakeHole at (vs : 'T []) hole =
      let nvs = Array.zeroCreate (vs.Length + 1)
      let rec idLoop c i =
        if i < vs.Length then
          if c = 0 then
            skipLoop i
          else
            nvs.[i] <- vs.[i]
            idLoop (c - 1) (i + 1)
      and skipLoop i =
        if i < vs.Length then
          nvs.[i + 1] <- vs.[i]
          skipLoop (i + 1)
      idLoop at 0
      nvs.[at] <- hole
      nvs

    let empty () = PersistentHashMap.Empty<_, _> ()

    let set k v (phm : PersistentHashMap<_, _>) = phm.Set (k, v)

    let length (phm : PersistentHashMap<_, _>) =
      let mutable l = 0
      let visitor _ _ = l <- l + 1; true
      phm.Visit (Func<_, _, _> visitor) |> ignore
      l

    let uniqueKey vs =
      vs
      |> FsLinq.groupBy fst
      |> FsLinq.map (fun g -> g.Key, (g |> FsLinq.map snd |> FsLinq.last))
      |> FsLinq.sortBy fst
      |> FsLinq.toArray

    let fromArray kvs =
      Array.fold
        (fun s (k, v) -> set k v s)
        (empty ())
        kvs

    let toArray (phm : PersistentHashMap<'K, 'V>) =
      phm
      |> FsLinq.map (fun kv -> kv.Key, kv.Value)
      |> FsLinq.toArray

    let toSortedKeyArray phm =
      let vs = phm |> toArray
      vs |> Array.sortInPlaceBy fst
      vs

    let checkInvariant (phm : PersistentHashMap<'K, 'V>) = phm.CheckInvariant ()

  open Common

  type ComplexType =
    | IntKey    of  int
    | StringKey of  int
    | TupleKey  of  int*string

  type HalfHash(v : int) =
    member x.Value = v

    interface IComparable<HalfHash> with
      member x.CompareTo(o : HalfHash)  = v.CompareTo o.Value

    interface IEquatable<HalfHash> with
      member x.Equals(o : HalfHash)  = v = o.Value

    override x.Equals(o : obj)  =
      match o with
      | :? HalfHash as k -> v = k.Value
      | _                -> false
    override x.GetHashCode()    = (v.GetHashCode ()) >>> 16 // In order to get a fair bunch of duplicated hashes
    override x.ToString()       = sprintf "%d" v

  type Action =
    | Add     of int*string
    | Remove  of int

  type Properties () =
    static member ``PopCount returns number of set bits`` (i : uint32) =
      let expected  = popCount i
      let actual    = PersistentHashMap.PopCount i

      expected      = actual

    static member ``CopyArray copies the array`` (vs : int []) =
      let expected  = vs
      let actual    = PersistentHashMap.CopyArray vs

      notIdentical expected actual
      && expected = actual

    static member ``CopyArrayMakeHoleLast copies the array and leaves a hole in last pos`` (vs : Empty []) (hole : Empty)=
      let expected  = Array.append vs [| hole |]
      let actual    = PersistentHashMap.CopyArrayMakeHoleLast (vs, hole)

      notIdentical expected actual
      && expected = actual

    static member ``CopyArrayMakeHole copies the array and leaves a hole at pos`` (at : int) (vs : Empty []) (hole : Empty)=
      let at        = abs at % (vs.Length + 1)
      let expected  = copyArrayMakeHole at vs hole
      let actual    = PersistentHashMap.CopyArrayMakeHole (at, vs, hole)

      notIdentical expected actual
      && expected = actual

    static member ``PHM toArray must contain all added values`` (vs : (int*string) []) =
      let expected  = uniqueKey vs
      let phm       = vs |> fromArray
      let actual    = phm |> toSortedKeyArray

      notIdentical expected actual
      && checkInvariant phm
      && expected = actual

    static member ``PHM TryFind must return all added values`` (vs : (ComplexType*ComplexType) []) =
      let unique    = uniqueKey vs
      let phm       = unique |> fromArray

      let rec loop i =
        if i < unique.Length then
          let k, v = unique.[i]
          match phm.TryFind k with
          | true, fv when fv = v  -> loop (i + 1)
          | _   , _               -> false
        else
          true

      checkInvariant phm
      && loop 0

    static member ``PHM Unset on all added values must yield empty map`` (vs : (HalfHash*Empty) []) =
      let unique    = uniqueKey vs
      let phm       = unique |> fromArray

      let rec loop (phm : PersistentHashMap<_, _>) i =
        if checkInvariant phm |> not then
          None
        elif i < unique.Length then
          if phm.IsEmpty then
            None
          else
            let k, v = unique.[i]
            loop (phm.Unset k) (i + 1)
        else
          Some phm

      match loop phm 0 with
      | Some phm  -> phm.IsEmpty
      | None      -> false

    static member ``PHM should behave as Map`` (vs : Action []) =
      let compare map (phm : PersistentHashMap<_, _>) =
        let empty =
          match map |> Map.isEmpty, phm.IsEmpty with
          | true  , true
          | false , false -> true
          | _     , _     -> false

        let visitor k v =
          match map |> Map.tryFind k with
          | Some fv -> v = fv
          | _       -> false

        checkInvariant phm && (length phm = map.Count) && empty && phm.Visit (Func<_, _, _> visitor)

      let ra = ResizeArray<int> ()

      let rec loop map (phm : PersistentHashMap<_, _>) i =
        if i < vs.Length then
          match vs.[i] with
          | Add (k, v)  ->
            ra.Add k
            let map = map |> Map.add k v
            let phm = phm.Set (k, v)
            compare map phm && loop map phm (i + 1)
          | Remove r    ->
            if ra.Count > 0 then
              let r   = abs r % ra.Count
              let k   = ra.[r]
              ra.RemoveAt r
              let map = map |> Map.remove k
              let phm = phm.Unset k
              compare map phm && loop map phm (i + 1)
            else
              loop map phm (i + 1)
        else
          true

      loop Map.empty (empty ()) 0

  let testLongInsert () =
#if DEBUG
    let count       = 1000
#else
    let count       = 1000000
#endif
    let multiplier  = 8
    printfn "testLongInsert: count:%d, multiplier:%d" count multiplier
    let random      = makeRandom 19740531
    let inserts     = [| for x in 1..count -> random 0 (count * multiplier) |]
    let removals    = shuffle random inserts

    let mutable phm = empty ()

    for i in inserts do
      phm <- phm.Set (i, i)
      match phm.TryFind i with
      | true, v when v = i  -> ()
      | _                   -> failwith "testLongInsert/insert/tryFind failed"

#if DEBUG
      if phm |> checkInvariant |> not then
        failwith "testLongInsert/insert/checkInvariant failed"
#endif

    for r in removals do
      phm <- phm.Unset r
      match phm.TryFind r with
      | false , _ -> ()
      | _         -> failwith "testLongInsert/remove/tryFind failed"

#if DEBUG
      if phm |> checkInvariant |> not then
        failwith "testLongInsert/remove/checkInvariant failed"
#endif

    printfn "  Done"

  let run () =
//    Properties.``PHM toArray must contain all added values`` [|(13, null); (-3, ""); (0, "")|] |> printfn "Result: %A"
    Check.All<Properties> FsCheckConfig.config
    testLongInsert ()

#if !DEBUG
module PerformanceTests =
  open PHM.CS

  open System
  open System.Diagnostics

  type Checker () =
    [<Conditional ("DEBUG")>]
    static member inline check fb str =
      if not (fb ()) then
        printfn "Check failed: %s" str
        failwith str

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

//  type Key = int
//  let makeKey i : int = i

  let random      = makeRandom 19740531
  let total       = 4000000
  let outer       = 40000
  let inner       = total / outer
  let multiplier  = 4
  let inserts     =
    [|
      for i in 0..(inner - 1) -> random 0 (inner*multiplier) |> makeKey, string i
    |]
  let removals    = shuffle random inserts
  let lookups     = shuffle random inserts

  module PersistentHashMap =
    let length (phm : PersistentHashMap<_, _>) =
      let mutable l = 0
      let visitor _ _ = l <- l + 1; true
      phm.Visit (Func<_, _, _> visitor) |> ignore
      l

    let inline doInsert phm =
      inserts
      |> Array.fold (fun (s : PersistentHashMap<_, _>) (k, v) -> s.Set (k, v)) phm

    let inline doRemove phm =
      removals
      |> Array.fold (fun (s : PersistentHashMap<_, _>) (k, _) -> s.Unset k) phm

    let inline doLookup fa (phm : PersistentHashMap<_, _>) =
      fa
      |> Array.forall (fun (k, _) -> let r, _ = phm.TryFind k in r)

    let empty     = PersistentHashMap.Empty<Key, string> ()
    let inserted  = doInsert empty

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> result.IsEmpty) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> result.IsEmpty) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module FsPersistentHashMap =
    open Persistent

    let inline doInsert phm =
      inserts
      |> Array.fold (fun s (k, v) -> PersistentHashMap.set k v s) phm

    let doRemove phm =
      removals
      |> Array.fold (fun s (k, v) -> PersistentHashMap.unset k s) phm

    let inline doLookup fa phm =
      fa
      |> Array.forall (fun (k, _) -> PersistentHashMap.containsKey k phm)

    let inserted  = doInsert PersistentHashMap.empty

    let insert () =
      let result    = doInsert PersistentHashMap.empty
      Checker.check (fun () -> PersistentHashMap.length result = PersistentHashMap.length inserted) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> PersistentHashMap.isEmpty result) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert PersistentHashMap.empty
      let result    = doRemove inserted
      Checker.check (fun () -> PersistentHashMap.isEmpty result) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert PersistentHashMap.empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module RedBlackTree =
    let inline doInsert hm =
      inserts
      |> Array.fold (fun s (k, v) -> s |> RedBlackTree.set k v) hm

    let inline doLookup fa hm =
      fa
      |> Array.forall (fun (k, _) -> hm |> RedBlackTree.containsKey k)

    let empty     = RedBlackTree.empty
    let inserted  = doInsert empty

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> RedBlackTree.count result = RedBlackTree.count inserted) "Expected to be same length as testSet"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module PrimeVmap =
    open Prime

    let inline doInsert phm =
      inserts
      |> Array.fold (fun s (k, v) -> Vmap.add k v s) phm

    let inline doRemove phm =
      removals
      |> Array.fold (fun s (k, v) -> Vmap.remove k s) phm

    let inline doLookup fa phm =
      fa
      |> Array.forall (fun (k, _) -> Vmap.containsKey k phm)

    let empty     = Vmap.makeEmpty ()

    let inserted  = doInsert empty

    let length vm = vm |> Vmap.toSeq |> Seq.length

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> Vmap.isEmpty result) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> Vmap.isEmpty result) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module ImmsMap =
    open Imms

    let inline doInsert phm =
      inserts
      |> Array.fold (fun s (k, v) -> ImmMap.set k v s) phm

    let inline doRemove phm =
      removals
      |> Array.fold (fun s (k, v) -> ImmMap.remove k s) phm

    let inline doLookup fa phm =
      fa
      |> Array.forall (fun (k, _) -> ImmMap.containsKey k phm)

    let empty     = ImmMap.empty

    let inserted  = doInsert empty

    let length vm = ImmMap.length vm

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> length result = length inserted) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> ImmMap.isEmpty result) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> ImmMap.isEmpty result) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module Map =
    open System.Collections.Generic

    let inline doInsert hm =
      inserts
      |> Array.fold (fun s (k, v) -> s |> Map.add k v) hm

    let inline doRemove hm =
      removals
      |> Array.fold (fun s (k, _) -> s |> Map.remove k) hm

    let inline doLookup fa hm =
      fa
      |> Array.forall (fun (k, _) -> hm |> Map.containsKey k)

    let empty     = Map.empty

    let inserted  = doInsert empty

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> result.Count = inserted.Count) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module Map2 =

    open Patches.Microsoft.FSharp.Collections

    open System.Collections.Generic

    let inline doInsert hm =
      inserts
      |> Array.fold (fun s (k, v) -> s |> Map.add k v) hm

    let inline doRemove hm =
      removals
      |> Array.fold (fun s (k, _) -> s |> Map.remove k) hm

    let inline doLookup fa hm =
      fa
      |> Array.forall (fun (k, _) -> hm |> Map.containsKey k)

    let empty     =
      let comparer =
        { new IComparer<Key> with
          member x.Compare (l, r) = l.Value.CompareTo r.Value
        }
      Map.emptyWithComparer comparer

    let inserted  = doInsert empty

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> result.Count = inserted.Count) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module FSharpx =
    open Patches.FSharpx.Collections

    let inline doInsert hm =
      inserts
      |> Array.fold (fun s (k, v) -> s |> PersistentHashMap.add k v) hm

    let inline doRemove hm =
      removals
      |> Array.fold (fun s (k, _) -> s |> PersistentHashMap.remove k) hm

    let inline doLookup fa hm =
      fa
      |> Array.forall (fun (k, _) -> hm |> PersistentHashMap.containsKey k)

    let empty     = PersistentHashMap<Key, string>.Empty ()
    let inserted  = doInsert empty

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> result.Length = inserted.Length) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> result.Length = 0) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> result.Length = 0) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module Dict =
    open System.Collections.Generic

    let dictAdd k v (d : Dictionary<_, _>) =
      let copy = Dictionary<_, _> d // Need to copy dictionary to preserve immutability
      copy.[k] <- v
      copy

    let dictRemove k (d : Dictionary<_, _>) =
      let copy = Dictionary<_, _> d // Need to copy dictionary to preserve immutability
      copy.Remove k |> ignore
      copy

    let inline dictContainsKey k (d : Dictionary<_, _>) =
      d.ContainsKey k

    let inline doInsert hm =
      inserts
      |> Array.fold (fun s (k, v) -> s |> dictAdd k v) hm

    let inline doRemove hm =
      removals
      |> Array.fold (fun s (k, _) -> s |> dictRemove k) hm

    let inline doLookup fa hm =
      fa
      |> Array.forall (fun (k, _) -> hm |> dictContainsKey k)

    let empty     = Dictionary<_, _> ()
    let inserted  =
      let dict = Dictionary<_, _> ()
      for k, v in inserts do
        dict.[k] <- v
      dict

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> result.Count = inserted.Count) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  module SCI =
    open System.Collections.Immutable

    let inline doInsert hm =
      inserts
      |> Array.fold (fun (s : ImmutableDictionary<_, _>) (k, v) -> (s.Remove k).Add (k, v)) hm

    let inline doRemove hm =
      removals
      |> Array.fold (fun (s : ImmutableDictionary<_, _>) (k, _) -> s.Remove k) hm

    let inline doLookup fa (hm : ImmutableDictionary<_, _>) =
      fa
      |> Array.forall (fun (k, _) -> hm.ContainsKey k)

    let empty     = ImmutableDictionary<Key, String>.Empty;
    let inserted  = doInsert empty

    let insert () =
      let result    = doInsert empty
      Checker.check (fun () -> result.Count = inserted.Count) "Expected to be same length as testSet"

    let remove () =
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndRemove () =
      let inserted  = doInsert empty
      let result    = doRemove inserted
      Checker.check (fun () -> result.Count = 0) "Expected to be empty"

    let insertAndLookup () =
      let inserted  = doInsert empty
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

    let lookupInserted () =
      let result    = doLookup lookups inserted
      Checker.check (fun () -> result) "Expected true for all"

  let testCases =
    [|
      "Lookup"  , "Mutable Dictionary"            , Dict.lookupInserted
      "Insert"  , "Mutable Dictionary"            , Dict.insert
      "Remove"  , "Mutable Dictionary"            , Dict.remove
      "Lookup"  , "Persistent Hash Map (C#)"      , PersistentHashMap.lookupInserted
      "Insert"  , "Persistent Hash Map (C#)"      , PersistentHashMap.insert
      "Remove"  , "Persistent Hash Map (C#)"      , PersistentHashMap.remove
      "Lookup"  , "Persistent Hash Map (F#)"      , FsPersistentHashMap.lookupInserted
      "Insert"  , "Persistent Hash Map (F#)"      , FsPersistentHashMap.insert
      "Remove"  , "Persistent Hash Map (F#)"      , FsPersistentHashMap.remove
      "Lookup"  , "Red Black Tree"                , RedBlackTree.lookupInserted
      "Insert"  , "Red Black Tree"                , RedBlackTree.insert
      "Lookup"  , "FSharpx.Collections"           , FSharpx.lookupInserted
      "Insert"  , "FSharpx.Collections"           , FSharpx.insert
      "Remove"  , "FSharpx.Collections"           , FSharpx.remove
      "Lookup"  , "Prime.Vmap"                    , PrimeVmap.lookupInserted
      "Insert"  , "Prime.Vmap"                    , PrimeVmap.insert
      "Remove"  , "Prime.Vmap"                    , PrimeVmap.remove
      "Lookup"  , "Imms.ImmMap"                   , ImmsMap.lookupInserted
      "Insert"  , "Imms.ImmMap"                   , ImmsMap.insert
      "Remove"  , "Imms.ImmMap"                   , ImmsMap.remove
      "Lookup"  , "System.Collections.Immutable"  , SCI.lookupInserted
      "Insert"  , "System.Collections.Immutable"  , SCI.insert
      "Remove"  , "System.Collections.Immutable"  , SCI.remove
      "Lookup"  , "FSharp.Collections.Map"        , Map.lookupInserted
      "Insert"  , "FSharp.Collections.Map"        , Map.insert
      "Remove"  , "FSharp.Collections.Map"        , Map.remove
      "Lookup"  , "FSharp.Collections.Map'"       , Map2.lookupInserted
      "Insert"  , "FSharp.Collections.Map'"       , Map2.insert
      "Remove"  , "FSharp.Collections.Map'"       , Map2.remove
    |]

  let run () =
    // printfn "%s" (PersistentHashMap.inserted.ToString ())
    use tw = new System.IO.StreamWriter "performance_results.csv"
    let line  l = tw.WriteLine (l : string)
    let linef f = FSharp.Core.Printf.kprintf line f
    line "Type,Name,TimeInMs,CC0,CC1,CC2"
    for tp, nm, a in testCases do
      printfn "Running test case: %s - %s..." tp nm
      let _, tm, cc0, cc1, cc2 = time outer a
      printfn "...It took %d ms, CC=%d, %d, %d" tm cc0 cc1 cc2
      linef "%s,%s,%d,%d,%d,%d" tp nm tm cc0 cc1 cc2
#endif

open System

[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
    FsPropertyTests.run ()
    PropertyTests.run ()
  #if !DEBUG
    PerformanceTests.run ()
  #endif
    0
  with
  | e ->
    printfn "Caught: %s" e.Message
    999
