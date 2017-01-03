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

module FsLinq

open System
open System.Linq

let inline first    source                        = Enumerable.First    (source)
let inline groupBy  (selector : 'T -> 'U) source  = Enumerable.GroupBy  (source, Func<'T, 'U> selector)
let inline last     source                        = Enumerable.Last     (source)
let inline map      (selector : 'T -> 'U) source  = Enumerable.Select   (source, Func<'T, 'U> selector)
let inline sortBy   (selector : 'T -> 'U) source  = Enumerable.OrderBy  (source, Func<'T, 'U> selector)
let inline toArray  source                        = Enumerable.ToArray  (source)
