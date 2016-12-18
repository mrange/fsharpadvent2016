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
module PullStream

// Maybe<'T> is like Option<'T> but it's a struct
[<Struct>]
[<NoComparison>]
[<NoEqualityAttribute>]
type Maybe<'T>(v : 'T, hasValue : bool) =
  member    x.Value        = v
  member    x.HasValue     = hasValue
  override  x.ToString ()  =
    if hasValue then
      sprintf "Just %A" v
    else
      "Nothing"

let Nothing<'T>     = Maybe<'T> (Unchecked.defaultof<'T>, false)
let inline Just v   = Maybe<'T> (v, true)

(*
// These are nice but unfortunately they added a bit overhead
let inline (|IsJust|IsNothing|) (ov : Maybe<'T>) =
  if ov.HasValue then
    IsJust ov.Value
  else
    IsNothing
*)

type Iterator<'T> = unit -> Maybe<'T>
type Stream<'T>   = unit -> Iterator<'T>

[<RequireQualifiedAccess>]
module Stream =

  module Details =
    let theEnd () = Nothing

    [<RequireQualifiedAccess>]
    module Loop =
      let rec filter f i =
        let v = i () : Maybe<_>
        if v.HasValue then
          if f v.Value then
            v
          else
            filter f i
        else
          Nothing

      let rec fold (f : OptimizedClosures.FSharpFunc<_, _, _>)  s i =
        let v = i () : Maybe<_>
        if v.HasValue then
          fold f (f.Invoke (s, v.Value)) i
        else
          s

      let rec skip r i =
        if r > 0 then
          let v = i () : Maybe<_>
          if v.HasValue then
            skip (r - 1) i
          else
            false
        else
          true

      let rec toArray (ra : ResizeArray<_>) i =
        let v = i () : Maybe<_>
        if v.HasValue then
          ra.Add v.Value
          toArray ra i


  open Details

  // Applies the given function to each element of the sequence and
  //  concatenates all the results.
  let inline collect (f : 'T -> Stream<'U>) (s : Stream<'T>) : Stream<'U> =
    fun () ->
      let i   = s ()

      let rec pop () =
        let v = i ()
        if v.HasValue then
          Just ((f v.Value) ())
        else
          Nothing

      let rii = ref (pop ())

      let rec loop () =
        let ii = !rii
        if ii.HasValue then
          let iv = ii.Value ()
          if iv.HasValue then
            iv
          else
            rii := pop ()
            loop ()
        else
          Nothing

      loop

  let inline bind t fu    = collect t fu

  let inline concat s     = collect id s

  let inline flatMap f s  = collect f s

  // An empty stream
  let empty<'T> : Stream<'T> =
    fun () ->
      theEnd

  // Returns a stream containing only the elements of the
  //  stream for which the given predicate returns "true"
  let inline filter (f : 'T -> bool) (s : Stream<'T>): Stream<'T> =
    fun () ->
      let i = s ()
      fun () ->
        Loop.filter f i

  // Applies a function to each element of the stream,
  //  threading an accumulator argument through the computation.
  //  If the input function is f and the elements are i0...iN
  //  then computes f (... (f s i0)...) iN
  let inline fold (f : 'S -> 'T -> 'S) (z : 'S) (s : Stream<'T>) : 'S =
    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
    let i = s ()
    Loop.fold f z i

  // Builds a new stream whose elements are the
  //  results of applying the given function to each of
  //  the elements of the stream.
  let inline map (f : 'T -> 'U) (s : Stream<'T>): Stream<'U> =
    fun () ->
      let i = s ()
      fun () ->
        let v = i ()
        if v.HasValue then
          Just (f v.Value)
        else
          Nothing

  // Builds a new stream whose elements are the
  //  results of applying the given function to each of
  //  the elements of the stream.
  let inline mapi (f : int -> 'T -> 'U) (s : Stream<'T>): Stream<'U> =
    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
    fun () ->
      let i = s ()
      let c = ref -1
      fun () ->
        let v = i ()
        if v.HasValue then
          incr c
          Just (f.Invoke (!c, v.Value))
        else
          Nothing

  // Creates a stream from an array
  let inline ofArray (vs : 'T []) : Stream<'T> =
    fun () ->
      let i = ref 0
      fun () ->
        if !i < vs.Length then
          let v = vs.[!i]
          incr i
          Just v
        else
          Nothing

  // Prints each element in the input stream as they are
  //  processed. Useful for debugging
  let inline print (name : string) (s : Stream<'T>) : Stream<'T> =
    fun () ->
      let i = s ()
      fun () ->
        let v = i ()
        printfn "Stream.print - %s : %A" name v
        v

  // Creates a stream containing all integer from
  //  first to last inclusive
  let inline range (first : int) (last : int) : Stream<int> =
    if first > last then
      empty
    else
      fun () ->
        let c = ref first
        fun () ->
          if !c <= last then
            let r = Just !c
            incr c
            r
          else
            Nothing

  // Creates a stream where v is replicated n times
  let inline replicate (n : int) (v : 'T) : Stream<'T> =
    let n = max 0 n
    if n = 0 then
      empty
    else
      fun () ->
        let c = ref n
        fun () ->
          if !c > 0 then
            decr c
            Just v
          else
            Nothing

  // Creates a stream containing the single element v
  let inline singleton (v : 'T) : Stream<'T> =
    fun () ->
      let cont = ref true
      fun () ->
        if !cont then
          cont := false
          Just v
        else
          Nothing

  let inline return_ v = singleton v

  // Returns a stream that skips N elements of the underlying
  //  stream and then yields the remaining elements of the
  //  stream
  let inline skip (n : int) (s : Stream<'T>): Stream<'T> =
    let n = max 0 n
    if n = 0 then
      s
    else
      fun () ->
        let i = s ()
        if Loop.skip n i then
          i
        else
          theEnd

  let inline sum (s : Stream<'T>) : 'T =
    let i = s ()
    let rec loop s =
      let v = i ()
      if v.HasValue then
        loop (s + v.Value)
      else
        s
    loop LanguagePrimitives.GenericZero

  // Returns the first N elements of the stream
  let inline take (n : int) (s : Stream<'T>): Stream<'T> =
    let n = max 0 n
    if n = 0 then
      empty
    else
      fun () ->
        let i = s ()
        let r = ref n
        fun () ->
          if !r > 0 then
            let v = i ()
            if v.HasValue then
              decr r
              v
            else
              Nothing
          else
            Nothing

  // Creates an array from a stream
  let inline toArray (s : Stream<'T>) : 'T [] =
    let ra  = ResizeArray<'T> 16
    let i   = s ()
    Loop.toArray ra i
    ra.ToArray ()

  // Returns the first element of the stream, or None if the stream is empty
  let inline tryFirst (s : Stream<'T>) : 'T option =
    let i = s ()
    let v = i ()
    if v.HasValue then
      Some v.Value
    else
      None
