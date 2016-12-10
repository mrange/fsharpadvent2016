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

module Common

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
