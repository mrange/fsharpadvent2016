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
namespace PushStream

type Receiver<'T> = 'T            -> bool

/// <summary>The type of the push stream.</summary>
type Stream<'T>   = Receiver<'T>  -> unit

/// <summary>Basic operations on streams.</summary>
[<RequireQualifiedAccess>]
module Stream =
  open System.Collections.Generic

  module Internals =

    let defaultSize       = 16
    let inline adapt s    = OptimizedClosures.FSharpFunc<_, _, _>.Adapt s

    let inline cbox<'T when 'T : not struct> (v : 'T) = box v

    let inline equality by =
      { new IEqualityComparer<_> with
        member this.Equals (x, y) =
          let xx = by x
          let yy = by y
          xx = yy
        member this.GetHashCode x =
          let xx = by x
          xx.GetHashCode ()
      }

    let inline comparer by =
      { new IComparer<_> with
        member this.Compare (x, y) =
          let xx = by x
          let yy = by y
          compare xx yy
      }

    [<RequireQualifiedAccess>]
    module Loop =
      // Function local loop functions currently make F# 4.0 create unnecessary loop objects
      //  By making loop functions external no unnecessary object is created
      //  In addition currently the pattern:
      //    let rec rangeForward s e r i = if i <= e then if r i then rangeForward s e r (i + s)
      //  Seems to perform better than equivalent pattern
      //    let rec rangeForward s e r i = if i <= e && r i then rangeForward s e r (i + s)
      //  Or:
      //    let rec rangeForward s e r i = let mutable i = i in while i <= e && r i do i <- i + s
      let rec resizeArrayForward (vs : ResizeArray<'T>) r i c = if i < c then if r vs.[i] then resizeArrayForward vs r (i + 1) c
      let rec resizeArrayReverse (vs : ResizeArray<'T>) r i = if i >= 0 then if r vs.[i] then resizeArrayReverse vs r (i - 1)
      let rec ofArray (vs : 'T []) r i = if i < vs.Length then if r vs.[i] then ofArray vs r (i + 1)
      let rec ofList r l =
        match l with
        | x::xs -> if r x then ofList r xs
        | _     -> ()
      let rec rangeForward s e r i = if i <= e then if r i then rangeForward s e r (i + s)
      let rec rangeReverse s e r i = if i >= e then if r i then rangeReverse s e r (i + s)
      let rec replicate n v r i = if i < n then if r v then replicate n v r (i + 1)
      let rec unfold f r s =
        match f s with
        | Some (v, s) -> if r v then unfold f r s
        | None        -> ()

  open Internals
  open System

  // sources

  /// <summary>Returns an empty stream of the given type.</summary>
  [<GeneralizableValue>]
  let empty<'T> : Stream<'T> =
    fun r ->
      ()

  /// <summary>Builds a stream from the given array.</summary>
  /// <param name="vs">The input array.</param>
  /// <returns>The stream of elements from the array.</returns>
  let inline ofArray (vs : 'T []) : Stream<'T> =
    fun r ->
      Loop.ofArray vs r 0

  /// <summary>Builds a stream from the given non-generic seq.</summary>
  /// <param name="vs">The input seq.</param>
  /// <returns>The stream of elements from the seq.</returns>
  let inline ofEnumerable (vs : System.Collections.IEnumerable) : Stream<obj> =
    fun r ->
      let mutable e = vs.GetEnumerator ()
      // Doesn't use tail-rec because e needs to be mutable in case of struct enumerators
      //  enumerables are slow anyway
      while e.MoveNext () && r e.Current do ()

  /// <summary>Builds a stream from the given list.</summary>
  /// <param name="vs">The input list.</param>
  /// <returns>The stream of elements from the list.</returns>
  let inline ofList (vs : 'T list) : Stream<'T> =
    fun r ->
      Loop.ofList r vs

  /// <summary>Builds a stream from the given ResizeArray.</summary>
  /// <param name="vs">The input ResizeArray.</param>
  /// <returns>The stream of elements from the ResizeArray.</returns>
  let inline ofResizeArray (vs : ResizeArray<'T>) : Stream<'T> =
    fun r ->
      Loop.resizeArrayForward vs r 0 vs.Count

  /// <summary>Builds a stream from the given seq.</summary>
  /// <param name="vs">The input seq.</param>
  /// <returns>The stream of elements from the seq.</returns>
  let inline ofSeq (vs : #seq<'T>) : Stream<'T> =
    fun r ->
      use mutable e = vs.GetEnumerator ()
      // Doesn't use tail-rec because e needs to be mutable in case of struct enumerators
      //  seqs are slow anyway
      while e.MoveNext () && r e.Current do ()

  /// <summary>Generates a stream from a range specification.</summary>
  /// <param name="b">The beginning of the range.</param>
  /// <param name="s">The step of the range.</param>
  /// <param name="e">The end of the range.</param>
  /// <exception cref="System.ArgumentException">Thrown when <c>s</c> is 0.</exception>
  /// <returns>The stream of generated elements.</returns>
  let inline range b s e : Stream<int> =
    if s = 0 then
      raise (ArgumentException ("Step of range can not be 0", "s"))
    elif b <= e && s > 0 then
      fun r ->
        Loop.rangeForward s e r b
    elif e <= b && s < 0 then
      fun r ->
        Loop.rangeReverse s e r b
    else
      empty

  /// <summary>Creates a stream by replicating the given initial value.</summary>
  /// <param name="n">The number of elements to replicate.</param>
  /// <param name="v">The value to replicate</param>
  /// <returns>The generated stream.</returns>
  let inline replicate n v : Stream<'T> =
    fun r ->
      Loop.replicate n v r 0

  /// <summary>Like fold, but computes on-demand and returns the stream of intermediary and final results.</summary>
  /// <param name="f">A function that updates the state with each element from the sequence.</param>
  /// <param name="z">The initial state.</param>
  /// <param name="s">The input stream </param>
  /// <returns>The resulting stream of computed states.</returns>
  let inline scan (f : 'S -> 'T -> 'S) (z : 'S) (s : Stream<'T>) : Stream<'S> =
    let f = adapt f
    fun r ->
      let mutable acc = z
      let mutable res = true
      s (fun v ->
          res <- r acc
          if res then acc <- f.Invoke (acc, v)
          res
        )
      if res then r acc |> ignore

  /// <summary>Returns a stream that contains one item only.</summary>
  /// <param name="v">The input item.</param>
  /// <returns>The stream of one item.</returns>
  let inline singleton v : Stream<'T> =
    fun r ->
      r v |> ignore

  /// <summary>Returns a stream that contains the elements generated by the given computation.
  /// The given initial <c>z</c> argument is passed to the element generator.</summary>
  /// <param name="f">A function that takes in the current state and returns an option tuple of the next
  /// element of the stream and the next state value.</param>
  /// <param name="z">The initial state value.</param>
  /// <returns>The result stream.</returns>
  let inline unfold (f : 'S -> ('T*'S) option) (z : 'S): Stream<'T> =
    fun r ->
      Loop.unfold f r z

  // pipes

  /// <summary>Wraps the two given streams as a single concatenated stream.</summary>
  /// <param name="ss">The second input stream.</param>
  /// <param name="fs">The first input stream.</param>
  /// <returns>The resulting strea.</returns>
  let inline append (ss : Stream<'T>) (fs : Stream<'T>) : Stream<'T> =
    fun r ->
      let mutable cont = true
      fs (fun v -> cont <- cont && r v; cont)
      if cont then
        ss r

  /// <summary>Applies the given function to each element of the stream. Returns
  /// the stream comprised of the results <c>x</c> for each element where
  /// the function returns Some(x)</summary>
  /// <param name="f">The function to generate options from the elements.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The stream comprising the values selected from the chooser function.</returns>
  let inline choose (f : 'T -> 'U option) (s : Stream<'T>) : Stream<'U> =
    fun r ->
      s (fun v -> match f v with Some u -> r u | None -> true)

  /// <summary>Divides the input stream into chunks of size at most <c>chunkSize</c>.</summary>
  /// <param name="size">The maximum size of each chunk.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The stream divided into chunks.</returns>
  let inline chunkBySize size (s : Stream<'T>) : Stream<'T []> =
    let size = max 1 size
    fun r ->
      let ra = ResizeArray size
      s (fun v ->
          ra.Add v
          if ra.Count < size then
            true
          else
            let vs = ra.ToArray ()
            ra.Clear ()
            r vs
        )
      if ra.Count > 0 then
        r (ra.ToArray ()) |> ignore
        ra.Clear ()

  /// <summary>For each element of the stream, applies the given function. Concatenates all the results and return the combined stream.</summary>
  /// <param name="m">The function to transform each input element into a substream to be concatenated.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The concatenation of the transformed substreams.</returns>
  let inline collect (m : 'T -> Stream<'U>) (s : Stream<'T>) : Stream<'U> =
    fun r ->
      let mutable cont = true
      s (fun v ->
          if cont then
            let ms = m v
            ms (fun v -> cont <- cont && r v; cont)
          cont
        )

  /// <summary>Traces each element in the stream in addition traces on completion. Used for debugging.</summary>
  /// <param name="name">A name traced with each value and completion.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The tracing stream.</returns>
  let inline debug name (s : Stream<'T>) : Stream<'T> =
    fun r ->
      s (fun v ->
          printfn "STREAM: %s - Value: %A" name v
          r v
        )
      printfn "STREAM: %s - Completed" name

  /// <summary>Returns a stream that contains no duplicate entries according to the
  /// generic hash and equality comparisons on the keys returned by the given key-generating function.
  /// If an element occurs multiple times in the stream then the later occurrences are discarded.</summary>
  /// <param name="by">A function transforming the stream items into comparable keys.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The result stream.</returns>
  let inline distinctBy (by : 'T -> 'U) (s : Stream<'T>) : Stream<'T> =
    fun r ->
      let seen = HashSet ()
      s (fun v -> if seen.Add (by v) then r v else true)
      seen.Clear ()

  /// <summary>Returns a stream that is the difference two streams with respect to the
  /// generic hash and equality comparisons on the keys returned by the given key-generating function.
  /// </summary>
  /// <param name="by">A function transforming the stream items into comparable keys.</param>
  /// <param name="ss">The second input stream.</param>
  /// <param name="fs">The first input stream.</param>
  /// <returns>The result stream.</returns>
  let exceptBy (by : 'T -> 'U) (ss : Stream<'T>) (fs : Stream<'T>) : Stream<'T> =
    fun r ->
      let seen = HashSet ()
      ss (fun v -> seen.Add (by v) |> ignore; true)
      fs (fun v -> if seen.Add (by v) then r v else true)

  /// <summary>Returns a new stream containing only the elements of the collection
  /// for which the given predicate returns "true"</summary>
  /// <param name="f">The function to test the input elements.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>A stream containing only the elements that satisfy the predicate.</returns>
  let inline filter (f : 'T -> bool) (s : Stream<'T>) : Stream<'T> =
    fun r ->
      s (fun v -> if f v then r v else true)

  /// <summary>Returns a stream that is the intersection of two streams with respect to the
  /// generic hash and equality comparisons on the keys returned by the given key-generating function.
  /// </summary>
  /// <param name="by">A function transforming the stream items into comparable keys.</param>
  /// <param name="ss">The first input stream.</param>
  /// <param name="fs">The second input stream.</param>
  /// <returns>The result stream.</returns>
  let inline intersectBy (by : 'T -> 'U) (ss : Stream<'T>) (fs : Stream<'T>) : Stream<'T> =
    fun r ->
      let seen = HashSet ()
      ss (fun v -> seen.Add (by v) |> ignore; true)
      fs (fun v -> if seen.Remove (by v) then r v && seen.Count > 0 else true)

  /// <summary>Builds a new stream whose elements are the results of applying the given function
  /// to each of the elements of the collection.</summary>
  /// <param name="m">The function to transform elements from the input stream.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The stream of transformed elements.</returns>
  let inline map (m : 'T -> 'U) (s : Stream<'T>) : Stream<'U> =
    fun r ->
      s (fun v -> r (m v))

  /// <summary>Combines map and fold. Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection</summary>
  /// <param name="m">The function to transform elements from the input collection and accumulate the final value.</param>
  /// <param name="z">The initial state.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The collection of transformed elements.</returns>
  let inline mapFold (m : 'S -> 'T -> 'U*'S) z (s : Stream<'T>) : Stream<'U> =
    fun r ->
      let mutable acc = z
      s (fun v -> let u, nacc = m acc v in acc <- nacc; r u)

  /// <summary>Builds a new stream whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.</summary>
  /// <param name="m">The function to transform elements and their indices.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The stream of transformed elements.</returns>
  let inline mapi (m : int -> 'T -> 'U) (s : Stream<'T>) : Stream<'U> =
    let m = adapt m
    fun r ->
      let mutable i = -1
      s (fun v -> i <- i + 1; r (m.Invoke (i, v)))

  /// <summary>Returns the greatest of all elements of the stream</summary>
  /// <param name="min">The initial value to compare against.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The largest element of the stream.</returns>
  let inline max min (s : Stream<'T>) : 'T =
    let mutable acc = min
    s (fun v -> acc <- max acc v; true)
    acc

  /// <summary>Returns the smallest of all elements of the stream</summary>
  /// <param name="min">The initial value to compare against.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The smallest element of the stream.</returns>
  let inline min max (s : Stream<'T>) : 'T =
    let mutable acc = max
    s (fun v -> acc <- min acc v; true)
    acc

  /// <summary>Returns a new stream containing only the elements of the collection
  /// that are of type 'U.</summary>
  /// <param name="s">The input stream.</param>
  /// <returns>A stream containing only the elements that are of type 'U.</returns>
  let inline tryCast (s : Stream<'T>) : Stream<'U> =
    fun r ->
      s (fun v ->
        match cbox v with
        | :? 'U as u -> r u
        | _          -> true
        )

  /// <summary>Returns a stream that is the union of two streams with respect to the
  /// generic hash and equality comparisons on the keys returned by the given key-generating function.
  /// </summary>
  /// <param name="by">A function transforming the stream items into comparable keys.</param>
  /// <param name="ss">The first input stream.</param>
  /// <param name="fs">The second input stream.</param>
  /// <returns>The result stream.</returns>
  let inline unionBy (by : 'T -> 'U) (ss : Stream<'T>) (fs : Stream<'T>) : Stream<'T> =
    fun r ->
      let seen          = HashSet ()
      let mutable cont  = true
      fs (fun v -> if seen.Add (by v) then cont <- cont && r v; cont else true)
      if cont then
        ss (fun v -> if seen.Add (by v) then r v else true)
      seen.Clear ()

  /// <summary>Returns a new stream with the elements in reverse order.</summary>
  /// <param name="s">The input stream.</param>
  /// <returns>The reversed stream.</returns>
  let inline rev (s : Stream<'T>) : Stream<'T> =
    fun r ->
      let ra = ResizeArray defaultSize
      s (fun v -> ra.Add v; true)
      Loop.resizeArrayReverse ra r (ra.Count - 1)
      ra.Clear ()

  /// <summary>Returns the stream after removing the first <c>n</c> elements.</summary>
  /// <param name="n">The number of elements to skip.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The stream after removing the first <c>n</c> elements.</returns>
  let inline skip n (s : Stream<'T>) : Stream<'T> =
    fun r ->
      let mutable rem = n
      s (fun v -> if rem > 0 then rem <- rem - 1; true else r v)

  /// <summary>Sorts the given stream using keys given by the given projection. Keys are compared using Operators.compare.</summary>
  /// <remarks>This is a stable sort, i.e. the original order of equal elements is preserved.</remarks>
  /// <param name="projection">The function to transform the stream elements into the type to be compared.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The sorted stream.</returns>
  let inline sortBy (by : 'T -> 'U) (s : Stream<'T>) : Stream<'T> =
    let c = comparer by
    fun r ->
      let ra = ResizeArray defaultSize
      s (fun v -> ra.Add v; true)
      ra.Sort c
      Loop.resizeArrayForward ra r 0 ra.Count
      ra.Clear ()

  /// <summary>Returns the first <c>n</c> elements of the stream.</summary>
  /// <param name="n">The number of items to take.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The result stream.</returns>
  let inline take n (s : Stream<'T>) : Stream<'T> =
    fun r ->
      let mutable rem = n
      s (fun v -> if rem > 0 then rem <- rem - 1; r v else false)

  // sinks

  /// <summary>Tests if at least one element of the stream satisfy the given predicate.</summary>
  /// <param name="f">A function to test an element of the input stream.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>True if at least one element of the stream satisfies the predicate; false otherwise.</returns>
  let inline exists (f : 'T -> bool) (s : Stream<'T>) : bool =
    let mutable acc = false
    s (fun v -> acc <- acc || f v; not acc)
    acc

  /// <summary>Returns the first element of the stream or <c>dv</c> if stream is empty.</summary>
  /// <param name="dv">The value to return if stream is empty.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The first element or <c>dv</c>.</returns>
  let inline first dv (s : Stream<'T>) : 'T =
    let mutable acc = dv
    s (fun v -> acc <- v; false)
    acc

  /// <summary>Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. Take the second argument, and apply the function to it
  /// and the first element of the stream. Then feed this result into the function along
  /// with the second element and so on. Return the final result.
  /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then
  /// computes <c>f (... (f s i0) i1 ...) iN</c>.</summary>
  /// <param name="f">The function to update the state given the input elements.</param>
  /// <param name="z">The initial state.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The final state value.</returns>
  let inline fold (f : 'S -> 'T -> 'S) (z : 'S) (s : Stream<'T>) : 'S =
    let f = adapt f
    let mutable acc = z
    s (fun v -> acc <- f.Invoke (acc, v); true)
    acc

  /// <summary>Tests if all elements of the stream satisfy the given predicate.</summary>
  /// <param name="f">A function to test an element of the input stream.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>True if every element of the stream satisfies the predicate; false otherwise.</returns>
  let inline forall (f : 'T -> bool) (s : Stream<'T>) : bool =
    let mutable acc = true
    s (fun v -> acc <- acc && f v; acc)
    acc

  /// <summary>Apply a function to each element of the collection, threading an accumulator argument
  /// through the computation. Apply the function to the first two elements of the stream.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes
  /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
  /// <param name="reduction">The function to reduce two stream elements to a single element.</param>
  /// <param name="s">The input stream.</param>
  /// <returns>The final reduced value.</returns>
  let inline reduce (r : 'T -> 'T -> 'T) (s : Stream<'T>) : 'T =
    let r = adapt r
    let mutable acc = LanguagePrimitives.GenericZero
    s (fun v -> acc <- r.Invoke (acc, v); true)
    acc

  /// <summary>Creates a sum of values in a given stream.</summary>
  /// <param name="s">The input stream.</param>
  /// <returns>The sum of all elements of the stream.</returns>
  let inline sum (s : Stream<'T>) : 'T =
    let mutable acc = LanguagePrimitives.GenericZero
    s (fun v -> acc <- acc + v; true)
    acc

  /// <summary>Builds an array from the given stream.</summary>
  /// <param name="s">The input stream.</param>
  /// <returns>The array containing the elements of the stream.</returns>
  let inline toArray (s : Stream<'T>) : 'T [] =
    let ra = ResizeArray defaultSize
    s (fun v -> ra.Add v; true)
    let vs = ra.ToArray ()
    ra.Clear ()
    vs

  /// <summary>Returns the first element of the stream or <c>None</c> if stream is empty.</summary>
  /// <param name="s">The input stream.</param>
  /// <returns>The first element or <c>None</c>.</returns>
  let inline tryFirst (s : Stream<'T>) : 'T option =
    let mutable acc = None
    s (fun v -> acc <- Some v; false)
    acc

  // aliases

  /// <summary>For each element of the stream, applies the given function. Concatenates all the results and return the combined stream.</summary>
  /// <param name="t">The input stream.</param>
  /// <param name="uf">The function to transform each input element into a substream to be concatenated.</param>
  /// <returns>The concatenation of the transformed substreams.</returns>
  let inline bind t uf = collect uf t

  /// <summary>Returns a stream that contains one item only.</summary>
  /// <param name="v">The input item.</param>
  /// <returns>The stream of one item.</returns>
  let inline return_ v = singleton v

  /// <summary>Returns a new stream that contains the elements of each the streams in order.</summary>
  /// <param name="s">The input stream of streams.</param>
  /// <returns>The resulting concatenated stream.</returns>
  let inline concat s  = collect id s