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

module RedBlackTree

type Color =
  | Red
  | Black
type Tree<'K, 'V> =
  | Leaf
  | Node  of Color*Tree<'K, 'V>*'K*'V*Tree<'K, 'V>

[<GeneralizableValue>]
let empty = Leaf

(*
let inline cmp (l : 'K) (r : 'K) =
  compare l r

type Comparer<'T when 'T : comparison> () =
  static let c = LanguagePrimitives.FastGenericComparer<'T>

  static member Comparer = c

let inline cmp (l : 'K) (r : 'K) =
  Comparer<'K>.Comparer.Compare (l, r)

*)

let inline cmp<'K when 'K :> System.IComparable<'K>> (l : 'K) (r : 'K) =
  l.CompareTo r

let rec containsKey k t =
  match t with
  | Leaf -> false
  | Node (_, ll, kk, __, rr) ->
    let cmp = cmp k kk
    if cmp < 0 then
      containsKey k ll
    elif cmp > 0 then
      containsKey k rr
    else
      true

let rec count t =
  match t with
  | Leaf -> 0
  | Node (_, ll, _, _, rr) ->
    count ll + 1 + count rr

let rec lookup k t =
  match t with
  | Leaf -> None
  | Node (_, ll, kk, vv, rr) ->
    let cmp = cmp k kk
    if cmp < 0 then
      lookup k ll
    elif cmp > 0 then
      lookup k rr
    else
      Some vv

let set k v t =
  let inline balance c l k v r =
    match c, l, k, v, r with
    | Black, Node (Red, Node (Red, a, xk, xv, b), yk, yv, c), zk, zv, d
    | Black, Node (Red, a, xk, xv, Node (Red, b, yk, yv, c)), zk, zv, d
    | Black, a, xk, xv, Node (Red, Node (Red, b, yk, yv, c), zk, zv, d)
    | Black, a, xk, xv, Node (Red, b, yk, yv, Node (Red, c, zk, zv, d)) ->
      Node (Red, Node (Black, a, xk, xv, b), yk, yv, Node (Black, c, zk, zv, d))
    | _ -> Node (c, l, k, v, r)
  let rec loop t =
    match t with
    | Leaf -> Node (Red, Leaf, k, v, Leaf)
    | Node (c, ll, kk, vv, rr)->
      let cmp = cmp k kk
      if cmp < 0 then
        balance c (loop ll) kk vv rr
      elif cmp > 0 then
        balance c ll kk vv (loop rr)
      else
        Node (c, ll, k, v, rr)
  match loop t with
  | Node (Red, ll, kk, vv, rr) -> Node (Black, ll, kk, vv, rr)
  | t -> t

let fromArray (vs : _ []) =
  let rec loop t i =
    if i < vs.Length then
      let k, v = vs.[i]
      let nt = set k v t
      loop nt (i + 1)
    else
      t
  loop empty 0

