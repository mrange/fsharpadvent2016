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
module PushPipe

type Receiver<'T>   = 'T    -> bool
type Resetter       = unit  -> unit
type Finalizer<'T>  = unit  -> 'T

type Pipe<'T, 'TInput>  = Receiver<'T> -> Receiver<'TInput>
type Sink<'T, 'TInput>  = Receiver<'TInput>*Resetter*Finalizer<'T>

module Pipe =

  module Internals =
    let defaultSize       = 16
    let inline adapt s    = OptimizedClosures.FSharpFunc<_, _, _>.Adapt s

    module Loop =
      let rec acceptArray (vs : 'T []) r i  = if i < vs.Length then r vs.[i] && acceptArray vs r (i + 1) else true
      let rec acceptRangeForward s e r i    = if i <= e then r i && acceptRangeForward s e r (i + s) else true
      let rec acceptRangeReverse s e r i    = if i >= e then r i && acceptRangeReverse s e r (i + s) else true

  open Internals
  open System

  let inline finalize (_, _, f)   = f ()
  let inline receive  (r, _, _) v = r v
  let inline reset    (_, r, _)   = r ()

  // sources

  [<GeneralizableValue>]
  let inline accept<'T> : Pipe<'T, 'T> =
    fun r v -> r v

  [<GeneralizableValue>]
  let inline acceptArray<'T> : Pipe<'T, 'T []> =
    fun r vs -> Loop.acceptArray vs r 0

  [<GeneralizableValue>]
  let inline acceptRange<'T> : Pipe<int, int*int*int> =
    fun r (b,s,e) ->
      if s = 0 then
        raise (ArgumentException ("Step of range can not be 0", "s"))
      elif b <= e && s > 0 then
        Loop.acceptRangeForward s e r b
      elif e <= b && s < 0 then
        Loop.acceptRangeReverse s e r b
      else
        true

  // pipes

  let inline filter (f : 'T -> bool) p : Pipe<'T, 'TInput> =
    fun r -> p (fun v -> if f v then r v else true)

  let inline map (m : 'T -> 'U) p : Pipe<'U, 'TInput> =
    fun r -> p (fun v -> r (m v))

  // sinks

  let inline toArray p : Sink<'T [], 'TInput> =
    let ra = ResizeArray defaultSize
    (p (fun v -> ra.Add v; true), (fun () -> ra.Clear ()), fun () -> ra.ToArray ())

  let inline sum p : Sink<'T, 'TInput> =
    let z = LanguagePrimitives.GenericZero
    let mutable acc = z
    (p (fun v -> acc <- acc + v; true), (fun () -> acc <- z), fun () -> acc)
