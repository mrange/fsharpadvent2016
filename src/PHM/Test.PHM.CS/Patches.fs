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

namespace Patches.Microsoft.FSharp.Collections

module LanguagePrimitives =
  let inline anyToString nullStr x = 
      match box x with 
      | null -> nullStr
      | :? System.IFormattable as f -> f.ToString(null,System.Globalization.CultureInfo.InvariantCulture)
      | obj ->  obj.ToString()

  let anyToStringShowingNull x = anyToString "null" x


module SR =

  let enumerationNotStarted       = "enumerationNotStarted"
  let enumerationAlreadyFinished  = "enumerationAlreadyFinished"
  let mapCannotBeMutated          = "mapCannotBeMutated"
  let notComparable               = "notComparable"
  let GetString s = s
  

