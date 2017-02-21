// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.Reflection
open System.IO

module Patches =
  let inline singleton k v = Map.empty.Add (k, v)

open Patches

[<AutoOpen>]
module Operators =

    /// The tautology function.
    /// No matter what you pass it, it evaluates to true.
    let tautology _ = true

    /// The tautology function with two arguments.
    /// No matter what you pass it, it evaluates to true.
    let tautology2 _ _ = true

    /// The tautology function with three arguments.
    /// No matter what you pass it, it evaluates to true.
    let tautology3 _ _ _ = true

    /// The absurdity function.
    /// No matter what you pass it, it evaluates to false.
    let absurdity _ = false

    /// The absurdity function with two arguments.
    /// No matter what you pass it, it evaluates to false.
    let absurdity2 _ _ = false

    /// Convert any value to an obj.
    let objectify x = x :> obj

    /// Curry up two values.
    let inline curry f x y = f (x, y)

    /// Uncurry two values.
    let inline uncurry f (x, y) = f x y

    /// Transforms a function by flipping the order of its arguments.
    let inline flip f x y = f y x

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f x y z = f z x y

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f x y z w = f w x y z

    /// Test for null.
    let inline isNull x = match x with null -> true | _ -> false

    /// Test for non-null.
    let inline isNotNull x = match x with null -> false | _ -> true

    /// Test that the given type has null as an actual value.
    let isNullTrueValue (ty : Type) =
        ty.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, true) |>
        Array.map (fun (attr : obj) -> attr :?> CompilationRepresentationAttribute) |>
        Array.exists (fun attr -> int attr.Flags &&& int CompilationRepresentationFlags.UseNullAsTrueValue <> 0)

    /// Convert a nullable value into an option.
    let inline denull x = match x with null -> None | _ -> Some x

    /// Test for string equality.
    let inline strEq str str2 = String.Equals (str, str2, StringComparison.Ordinal)

    /// Compare two strings.
    let inline strCmp str str2 = String.Compare (str, str2, StringComparison.Ordinal)

    /// Get the .NET type of a target.
    let inline getType target = target.GetType ()

    /// Get the .NET type name of a target.
    let inline getTypeName target = (getType target).Name

    /// Get the fields of a type.
    let inline getFields (t : Type) = t.GetFields (BindingFlags.Instance ||| BindingFlags.Public)

    /// Get the value of a field.
    let inline getFieldValue (f : FieldInfo) (x : obj) = f.GetValue x

    /// Get the properties of a type.
    let inline getProperties (t : Type) = t.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)

    /// Get the value of a property.
    let inline getPropertyValue indices (p : PropertyInfo) (x : obj) = p.GetValue (x, indices)

    /// Test for reference equality.
    let inline refEq (x : 'a) (y : 'a) = obj.ReferenceEquals (x, y)

    /// Test just the value parts of a type for equality.
    /// NOTE: This function uses mad reflection, so is extremely slow, and should not be used in tight loops.
    let rec similar (x : obj) (y : obj) =
        if isNull x then isNull y
        elif isNull y then false
        elif refEq (getType x) (getType y) then
            let ty = getType x
            if  ty.IsValueType ||
                ty = typeof<string> then
                x = y
            else if ty.IsSubclassOf typeof<Stream> then
                // NOTE: Stream has a screwed up contract that its Length property can throw if seeking is not
                // supported. They should have returned nullable int instead, but nooooo....
                true
            else
                let fieldsSimilar =
                    ty
                    |> getFields
                    |> Array.forall (fun i -> similar (getFieldValue i x) (getFieldValue i y))
                let propertiesSimilar =
                    ty
                    |> getProperties
                    |> Array.filter (fun p -> (p.GetIndexParameters ()).Length = 0)
                    |> Array.forall (fun i -> similar (getPropertyValue null i x) (getPropertyValue null i y))
                fieldsSimilar && propertiesSimilar
        else false

    /// Apply a function recursively a number of times.
    let rec doTimes fn arg times =
        if times < 0 then failwith "Cannot call doTimes with times < 0."
        elif times = 0 then arg
        else doTimes fn (fn arg) (times - 1)

    /// Perform an operation until a predicate passes.
    let rec doUntil op pred =
        if not (pred ()) then
            op ()
            doUntil op pred

    /// Add a custom TypeConverter to an existing type.
    let assignTypeConverter<'t, 'c> () =
        ignore (TypeDescriptor.AddAttributes (typeof<'t>, TypeConverterAttribute typeof<'c>))

    /// Short-hand for linq enumerable cast.
    let inline enumerable<'a> enumeratable =
        System.Linq.Enumerable.Cast<'a> enumeratable

    /// Get the enumerator for a sequence.
    let inline enumerator (enumeratable : _ seq) =
        enumeratable.GetEnumerator ()

    /// Determine if a string is a guid.
    let inline isGuid str =
        fst (Guid.TryParse str)

    /// Make a Guid.
    let inline makeGuid () =
        Guid.NewGuid ()

    /// Make a Guid from a couple of ints.
    /// It is the user's responsibility to ensure uniqueness when using the resulting Guids.
    let makeGuidFromInts m n =
        let bytes = Array.create<byte> 8 (byte 0)
        Guid (m, int16 (n >>> 16), int16 n, bytes)

    /// Fail with an unexpected match failure.
    let failwithumf () =
        let stackTrace = StackTrace ()
        let frame = stackTrace.GetFrame 1
        let meth = frame.GetMethod ()
        let line = frame.GetFileLineNumber ()
        let fileName = frame.GetFileName ()
        failwithf "Unexpected match failure in '%s' on line %i in file %s." meth.Name line fileName

    /// As close as we can get to F# implicits.
    let inline implicit arg = (^a : (static member op_Implicit : ^b -> ^a) arg)

    /// Sequences two functions like Haskell ($).
    let inline (^) f g = f g

/// An option type that doesn't generate garbage.
/// TODO: document.
type FOption<'a> =
    struct
        val private Exists : bool
        val private ValueOpt : 'a
        private new (_ : unit) = { Exists = false; ValueOpt = Unchecked.defaultof<'a> }
        private new (_ : unit, value : 'a) = { Exists = true; ValueOpt = value }
        static member Some (value : 'a) = new FOption<'a> ((), value)
        static member None () = new FOption<'a> (())
        static member FromOpt (opt : 'a option) = match opt with Option.Some value -> FOption.Some value | None -> FOption.None ()
        static member ToOpt (fopt : FOption<'a>) = if fopt.Exists then Some fopt.ValueOpt else None
        member this.IsSome = this.Exists
        member this.IsNone = not this.Exists
        member this.Value = if this.Exists then this.ValueOpt else failwith "FOption has no value to get."
        member this.Map (fn : 'a -> 'b) = if this.Exists then new FOption<'b> ((), fn this.ValueOpt) else new FOption<'b> (())
        end

[<RequireQualifiedAccess>]
module FOption =

    let fromOpt opt = FOption.FromOpt opt
    let toOpt fopt = FOption.ToOpt fopt
    let some value = FOption.Some value
    let none () = FOption.None ()
    let isSome (fopt : FOption<'a>) = fopt.IsSome
    let isNone (fopt : FOption<'a>) = fopt.IsNone
    let get (fopt : FOption<'a>) = fopt.Value
    let map fn (fopt : FOption<'a>) = fopt.Map fn

[<AutoOpen>]
module HMapModule =

    /// A hash-key-value triple, implemented with a struct for efficiency.
    type private Hkv<'k, 'v when 'k :> IEquatable<'k>> =
        struct
            new (b, h, k, v) = { B = b; H = h; K = k; V = v }
            val B : bool
            val H : int
            val K : 'k
            val V : 'v
            end

    /// TODO: P1: there's an F# issue where UseNullAsTrueValue does not work on unions with 4 or
    /// more cases https://github.com/Microsoft/visualfsharp/issues/711 . Once resolved, should use
    /// it and be able to make arrays with Array.zeroCreate alone without also copying over the
    /// empty array.
    type [<NoComparison>] private HNode<'k, 'v when 'k :> IEquatable<'k>> =
        | Nil
        | Singleton of Hkv<'k, 'v>
        | Multiple of HNode<'k, 'v> array
        | Gutter of Hkv<'k, 'v> array

    [<RequireQualifiedAccess>]
    module private HNode =

        let inline failwithKeyNotFound (k : 'k) =
            raise ^ KeyNotFoundException ^ "Could not find HMap key '" + k.ToString () + "'."

        /// OPTIMIZATION: Array.Clone () is not used since it's been profiled to be slower
        let inline private cloneArray (arr : HNode<'k, 'v> array) =
            let arr' = Array.zeroCreate 16 : HNode<'k, 'v> array    // NOTE: there's an unecessary check against the size here, but that's the only inefficiency
                                                                    // TODO: P1: use Array.zeroCreateUnchecked if / when it becomes available
            Array.Copy (arr, 0, arr', 0, 16) // param checks are inefficient, but hopefully there's at least a memcpy underneath...
            arr'
    
        let inline private hashToIndex h dep =
            (h >>> (dep * 4)) &&& 0xF

        let private addToGutter (entry : Hkv<'k, 'v>) (gutter : Hkv<'k, 'v> array) =
            let gutterLength = gutter.Length
            let gutter2 = Array.zeroCreate 16 : Hkv<'k, 'v> array
            Array.Copy (gutter, 0, gutter2, 0, gutterLength)
            gutter2.[gutterLength] <- entry
            gutter2

        let private removeFromGutter (k : 'k) (gutter : Hkv<'k, 'v> array) =
            match Array.FindLastIndex (gutter, fun (entry2 : Hkv<'k, 'v>) -> entry2.B && entry2.K.Equals k) with
            | -1 -> gutter
            | index ->
                let gutter2 = Array.zeroCreate (gutter.Length - 1) : Hkv<'k, 'v> array
                Array.Copy (gutter, 0, gutter2, 0, index)
                Array.Copy (gutter, index + 1, gutter2, index, gutter2.Length - index)
                gutter2

        let private tryFindInGutter (k : 'k) (gutter : Hkv<'k, 'v> array) =
            match Array.FindLastIndex (gutter, fun (entry2 : Hkv<'k, 'v>) -> entry2.B && entry2.K.Equals k) with
            | -1 -> FOption.none ()
            | index -> FOption.some gutter.[index].V

        let empty =
            Nil

        let isEmpty node =
            match node with
            | Nil -> true
            | _ -> false
    
        /// OPTIMIZATION: Requires an empty array to use the source of new array clones in order to avoid Array.create.
        let rec add (hkv : Hkv<'k, 'v>) (earr : HNode<'k, 'v> array) (dep : int) (node : HNode<'k, 'v>) : HNode<'k, 'v> =
    
            // lower than max depth, non-clashing
            if dep < 9 then
    
                // handle non-clash cases
                match node with
                | Nil ->
    
                    // make singleton entry
                    Singleton hkv
    
                | Singleton hkv' ->
                    
                    // if additional entry; convert Singleton to Multiple
                    let idx = hashToIndex hkv.H dep
                    let idx' = hashToIndex hkv'.H dep
                    if idx <> idx' then
                        let arr = cloneArray earr
                        arr.[idx] <- Singleton hkv
                        arr.[idx'] <- Singleton hkv'
                        Multiple arr
    
                    // if replace entry; remain Singleton
                    elif hkv.K.Equals hkv'.K then
                        Singleton hkv
    
                    // if add entry with same idx; add both in new node
                    else
                        let dep' = dep + 1
                        let node' = add hkv earr dep' Nil
                        let node' = add hkv' earr dep' node'
                        let arr = cloneArray earr
                        arr.[idx] <- node'
                        Multiple arr
    
                | Multiple arr ->
    
                    // add entry with recursion
                    let idx = hashToIndex hkv.H dep
                    let entry = arr.[idx]
                    let arr = cloneArray arr
                    arr.[idx] <- add hkv earr (dep + 1) entry
                    Multiple arr
    
                | Gutter _ ->
    
                    // logically should never hit here
                    failwithumf ()
    
            // clashing
            else
                
                // handle clash cases
                match node with
                | Nil -> Gutter (Array.singleton hkv)
                | Singleton hkv' -> Gutter [|hkv'; hkv|]
                | Multiple _ -> failwithumf () // should never hit here
                | Gutter gutter -> Gutter (addToGutter hkv gutter)
    
        let rec remove (h : int) (k : 'k) (dep : int) (node : HNode<'k, 'v>) : HNode<'k, 'v> =
            match node with
            | Nil -> node
            | Singleton hkv -> if hkv.K.Equals k then Nil else node
            | Multiple arr ->
                let idx = hashToIndex h dep
                let entry = arr.[idx]
                let arr = cloneArray arr
                arr.[idx] <- remove h k (dep + 1) entry
                if Array.forall isEmpty arr then Nil else Multiple arr // does not collapse Multiple to Singleton, tho could?
            | Gutter gutter ->
                let gutter = removeFromGutter k gutter
                if Array.isEmpty gutter then Nil else Gutter gutter
    
        let rec tryFindFast (h : int) (k : 'k) (dep : int) (node : HNode<'k, 'v>) : 'v FOption =
            match node with
            | Nil -> FOption.none ()
            | Singleton hkv -> if hkv.K.Equals k then FOption.some hkv.V else FOption.none ()
            | Multiple arr -> let idx = hashToIndex h dep in tryFindFast h k (dep + 1) arr.[idx]
            | Gutter gutter -> tryFindInGutter k gutter
    
        let rec find (h : int) (k : 'k) (dep : int) (node : HNode<'k, 'v>) : 'v =
            match node with
            | Nil -> failwithKeyNotFound k
            | Singleton hkv -> if hkv.K.Equals k then hkv.V else failwithKeyNotFound k
            | Multiple arr -> let idx = hashToIndex h dep in find h k (dep + 1) arr.[idx]
            | Gutter gutter -> FOption.get (tryFindInGutter k gutter)
    
        let rec fold folder state node =
            match node with
            | Nil -> state
            | Singleton hkv -> folder state hkv.K hkv.V
            | Multiple arr -> Array.fold (fold folder) state arr
            | Gutter gutter -> Array.fold (fun state (hkv : Hkv<_, _>) -> folder state hkv.K hkv.V) state gutter
    
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        let rec toSeq node =
            seq {
                match node with
                | Nil -> yield! Seq.empty
                | Singleton hkv -> yield (hkv.K, hkv.V)
                | Multiple arr -> for n in arr do yield! toSeq n
                | Gutter gutter -> yield! Array.map (fun (hkv : Hkv<_, _>) -> (hkv.K, hkv.V)) gutter }

    /// A fast persistent hash map.
    /// Works in effectively constant-time for look-ups and updates.
    type [<NoComparison>] HMap<'k, 'v when 'k :> IEquatable<'k>> =
        private
            { Node : HNode<'k, 'v>
              EmptyArray : HNode<'k, 'v> array }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () = (HNode.toSeq this.Node).GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () = (HNode.toSeq this.Node).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module HMap =
    
        /// Create an empty HMap.
        let makeEmpty () =
            { Node = HNode.empty
              EmptyArray = Array.create 16 HNode.empty }

        /// Check that an HMap is empty.
        let isEmpty map =
            HNode.isEmpty map.Node
    
        /// Check that an HMap is empty.
        let notEmpty map =
            not ^ HNode.isEmpty map.Node
    
        /// Add a value with the key to an HMap.
        let add (key : 'k) (value : 'v) map =
            let hkv = Hkv (true, key.GetHashCode (), key, value)
            let node = HNode.add hkv map.EmptyArray 0 map.Node
            { map with Node = node }
    
        /// Remove a value with the given key from an HMap.
        let remove (key : 'k) map =
            let h = key.GetHashCode ()
            { map with Node = HNode.remove h key 0 map.Node }
    
        /// Add all the given entries to an HMap.
        let addMany entries map =
            Seq.fold (fun map (key : 'k, value : 'v) -> add key value map) map entries
    
        /// Remove all values with the given keys from an HMap.
        let removeMany keys map =
            Seq.fold (fun map (key : 'k) -> remove key map) map keys
    
        /// Try to find a value with the given key in an HMap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        let tryFindFast (key : 'k) map : 'v FOption =
            let h = key.GetHashCode ()
            HNode.tryFindFast h key 0 map.Node

        /// Try to find a value with the given key in an HMap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        let tryFind (key : 'k) map : 'v option =
            let fopt = tryFindFast key map
            FOption.toOpt fopt

        /// Find a value with the given key in an HMap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.GetValue.
        let find (key : 'k) map : 'v =
            let h = key.GetHashCode ()
            HNode.find h key 0 map.Node
    
        /// Check that an HMap contains a value with the given key.
        let containsKey key map =
            let opt = tryFindFast key map
            FOption.isSome opt
            
        /// Combine the contents of two HMaps, taking an item from the second map in the case of a key conflict.
        let concat map map2 =
            Seq.fold (flip ^ uncurry add) map map2
    
        /// Fold over an HMap.
        let fold folder state (map : HMap<'k, 'v>) =
            HNode.fold folder state map.Node
    
        /// Map over an HMap.
        let map mapper (map : HMap<'k, 'v>) =
            fold
                (fun state key value -> add key (mapper value) state)
                (makeEmpty ())
                map
    
        /// Filter an HMap.
        let filter pred (map : HMap<'k, 'v>) =
            fold
                (fun state key value -> if pred key value then add key value state else state)
                (makeEmpty ())
                map
    
        /// Convert an HMap to a sequence of pairs of keys and values.
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        /// Don't use it unless you need its laziness or if performance won't be affected significantly.
        let toSeq (map : HMap<'k, 'v>) =
            map :> IEnumerable<'k * 'v>
    
        /// Convert a sequence of keys and values to an HMap.
        let ofSeq pairs =
            Seq.fold
                (fun map (key, value) -> add key value map)
                (makeEmpty ())
                pairs

/// A very fast persistent hash map.
/// Works in effectively constant-time for look-ups and updates.
type HMap<'k, 'v when 'k :> IEquatable<'k>> = HMapModule.HMap<'k, 'v>