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

    /// Test for equality, usually faster than (=).
    let inline fastEq (x : 'a) (y : 'a) = LanguagePrimitives.GenericEquality x y

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

    /// Test for equality, usually faster than (=).
    let inline (==) (x : 'a) (y : 'a) = fastEq x y

    /// Test just the value parts of a type for equality. Reflective and slow.
    let inline (===) (x : 'a) (y : 'a) = similar x y


/// A hash-key-value triple, implemented with a struct for efficiency.
type internal Hkv<'k, 'v when 'k : comparison> =
    struct
        new (h, k, v) = { H = h; K = k; V = v }
        val H : int
        val K : 'k
        val V : 'v
        end

[<AutoOpen>]
module VmapModule =

    /// TODO: there's an F# issue where UseNullAsTrueValue does not work on unions with 4 or more cases
    /// https://github.com/Microsoft/visualfsharp/issues/711 . Once resolved, should use it and be able
    /// to make arrays with Array.zeroCreate alone without also copying over the empty array.
    type [<NoComparison>] private Vnode<'k, 'v when 'k : comparison> =
        | Nil
        | Singleton of Hkv<'k, 'v>
        | Multiple of Vnode<'k, 'v> array
        | Clash of Map<'k, 'v>

    [<RequireQualifiedAccess>]
    module private Vnode =

        let inline failwithKeyNotFound (k : 'k) =
            raise ^ KeyNotFoundException ^ "Could not find Vmap key '" + k.ToString () + "'."

        /// OPTIMIZATION: Array.Clone () is not used since it's been profiled to be slower
        let inline cloneArray (arr : Vnode<'k, 'v> array) : Vnode<'k, 'v> array =
            let arr' = Array.zeroCreate 32  // NOTE: there's an unecessary check against the size here, but that's the only inefficiency
                                            // TODO: use Array.zeroCreateUnchecked if / when it becomes available
            Array.Copy (arr, 0, arr', 0, 32) // param checks are inefficient, but hopefully there's at least a memcpy underneath...
            arr'

        let inline private hashToIndex h dep =
            (h >>> (dep * 5)) &&& 0x1F

        let isEmpty node =
            match node with
            | Nil -> true
            | _ -> false

        let rec fold folder state node =
            match node with
            | Nil -> state
            | Singleton hkv -> folder state hkv.K hkv.V
            | Multiple arr -> Array.fold (fold folder) state arr
            | Clash clashMap -> Map.fold folder state clashMap

        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        let rec toSeq node =
            seq {
                match node with
                | Nil -> yield! Seq.empty
                | Singleton hkv -> yield (hkv.K, hkv.V)
                | Multiple arr -> for n in arr do yield! toSeq n
                | Clash clashMap -> yield! Map.toSeq clashMap }

        /// OPTIMIZATION: Requires an empty array to use the source of new array clones in order to avoid Array.create.
        let rec add (hkv : Hkv<'k, 'v>) (earr : Vnode<'k, 'v> array) (dep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =

            // lower than max depth, non-clashing
            if dep < 8 then

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
                    elif hkv.K == hkv'.K then
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

                | Clash _ ->

                    // logically should never hit here
                    failwithumf ()

            // clashing
            else

                // handle clash cases
                match node with
                | Nil -> Clash ^ singleton hkv.K hkv.V
                | Singleton hkv' -> Clash ^ Map.add hkv.K hkv.V ^ singleton hkv'.K hkv'.V
                | Multiple _ -> failwithumf () // should never hit here
                | Clash clashMap -> Clash ^ Map.add hkv.K hkv.V clashMap

        let rec remove (h : int) (k : 'k) (dep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =
            match node with
            | Nil -> node
            | Singleton hkv -> if hkv.K == k then Nil else node
            | Multiple arr ->
                let idx = hashToIndex h dep
                let entry = arr.[idx]
                let arr = cloneArray arr
                arr.[idx] <- remove h k (dep + 1) entry
                if Array.forall isEmpty arr then Nil else Multiple arr // does not collapse Multiple to Singleton, tho could?
            | Clash clashMap ->
                let clashMap = Map.remove k clashMap
                if Map.isEmpty clashMap then Nil else Clash clashMap

        let rec tryFind (h : int) (k : 'k) (dep : int) (node : Vnode<'k, 'v>) : 'v option =
            match node with
            | Nil -> None
            | Singleton hkv -> if hkv.K == k then Some hkv.V else None
            | Multiple arr -> let idx = hashToIndex h dep in tryFind h k (dep + 1) arr.[idx]
            | Clash clashMap -> Map.tryFind k clashMap

        let rec find (h : int) (k : 'k) (dep : int) (node : Vnode<'k, 'v>) : 'v =
            match node with
            | Nil -> failwithKeyNotFound k
            | Singleton hkv -> if hkv.K == k then hkv.V else failwithKeyNotFound k
            | Multiple arr -> let idx = hashToIndex h dep in find h k (dep + 1) arr.[idx]
            | Clash clashMap -> Map.find k clashMap

        let empty =
            Nil

    /// A very fast persistent hash map.
    /// Works in effectively constant-time for look-ups and updates.
    type [<NoComparison>] Vmap<'k, 'v when 'k : comparison> =
        private
            { Node : Vnode<'k, 'v>
              EmptyArray : Vnode<'k, 'v> array }

        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator ()

        interface IEnumerable with
            member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module Vmap =

        /// Create an empty Vmap.
        let makeEmpty () =
            { Node = Vnode.empty
              EmptyArray = Array.create 32 Vnode.empty }

        /// Check that a Vmap is empty.
        let isEmpty map =
            Vnode.isEmpty map.Node

        /// Check that a Vmap is empty.
        let notEmpty map =
            not ^ Vnode.isEmpty map.Node

        /// Add a value with the key to a Vmap.
        let add (key : 'k) (value : 'v) map =
            let hkv = Hkv (key.GetHashCode (), key, value)
            let node = Vnode.add hkv map.EmptyArray 0 map.Node
            { map with Node = node }

        /// Remove a value with the given key from a Vmap.
        let remove (key : 'k) map =
            let h = key.GetHashCode ()
            { map with Node = Vnode.remove h key 0 map.Node }

        /// Add all the given entries to a Vmap.
        let addMany entries map =
            Seq.fold (fun map (key : 'k, value : 'v) -> add key value map) map entries

        /// Remove all values with the given keys from a Vmap.
        let removeMany keys map =
            Seq.fold (fun map (key : 'k) -> remove key map) map keys

        /// Try to find a value with the given key in a Vmap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        let tryFind (key : 'k) map : 'v option =
            let h = key.GetHashCode ()
            Vnode.tryFind h key 0 map.Node

        /// Find a value with the given key in a Vmap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.GetValue.
        let find (key : 'k) map : 'v =
            let h = key.GetHashCode ()
            Vnode.find h key 0 map.Node

        /// Check that a Vmap contains a value with the given key.
        let containsKey key map =
            match tryFind key map with
            | Some _ -> true
            | None -> false

        /// Combine the contents of two Vmaps, taking an item from the second map in the case of a key conflict.
        let concat map map2 =
            Seq.fold (flip ^ uncurry add) map map2

        /// Fold over a Vmap.
        let fold folder state (map : Vmap<'k, 'v>) =
            Vnode.fold folder state map.Node

        /// Map over a Vmap.
        let map mapper (map : Vmap<'k, 'v>) =
            fold
                (fun state key value -> add key (mapper value) state)
                (makeEmpty ())
                map

        /// Filter a Vmap.
        let filter pred (map : Vmap<'k, 'v>) =
            fold
                (fun state key value -> if pred key value then add key value state else state)
                (makeEmpty ())
                map

        /// Convert a Vmap to a sequence of pairs of keys and values.
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        /// Don't use it unless you need its laziness or if performance won't be affected significantly.
        let toSeq (map : Vmap<'k, 'v>) =
            map :> IEnumerable<'k * 'v>

        /// Convert a sequence of keys and values to a Vmap.
        let ofSeq pairs =
            Seq.fold
                (fun map (key, value) -> add key value map)
                (makeEmpty ())
                pairs

/// A very fast persistent hash map.
/// Works in effectively constant-time for look-ups and updates.
type Vmap<'k, 'v when 'k : comparison> = VmapModule.Vmap<'k, 'v>