namespace SeqComposer.Microsoft.FSharp.Patches

  open System

  module SR = 
    let GetString s = s

    let enumerationAlreadyFinished  = ""
    let enumerationNotStarted       = ""
    let enumerationPastIntMaxValue  = ""
    let inputMustBeNonNegative      = ""
    let inputMustBePositive         = ""
    let inputSequenceTooLong        = ""
    let keyNotFoundAlt              = ""
    let notEnoughElements           = ""
    let resetNotSupported           = ""

  [<AutoOpen>]
  module DetailedExceptions =
    let inline invalidArgFmt (arg:string) (format:string) paramArray =    
        let msg = String.Format (format,paramArray)
        raise (new ArgumentException (msg,arg))

    let inline invalidOpFmt (format:string) paramArray =
        let msg = String.Format (format,paramArray)
        raise (new InvalidOperationException(msg))

    let inline invalidArgInputMustBeNonNegative (arg:string) (count:int) =
        invalidArgFmt arg "{0}\n{1} = {2}" [|LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString ; arg; count|]

  module Array =
    let inline stableSortInPlaceBy f a =
      Array.sortInPlaceBy f a

    let inline stableSortInPlace a =
      Array.sortInPlace a

    let inline stableSortInPlaceWith c a =
      Array.sortInPlaceWith c a

    let inline zeroCreateUnchecked c = Array.zeroCreate c

    let inline subUnchecked startIndex count (array : 'T[]) =
        let res = zeroCreateUnchecked count : 'T[]
        if count < 64 then
            for i = 0 to res.Length-1 do
                res.[i] <- array.[startIndex+i]
        else
            Array.Copy(array, startIndex, res, 0, count)
        res

    let scanSubRight f (array : _[]) start fin initState =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        let mutable state = initState
        let res = zeroCreateUnchecked (fin-start+2)
        res.[fin - start + 1] <- state
        for i = fin downto start do
            state <- f.Invoke(array.[i], state)
            res.[i - start] <- state
        res



// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace SeqComposer.Microsoft.FSharp.Collections
    #nowarn "52" // The value has been copied to ensure the original is not mutated by this operation

    open System
    open System.Diagnostics
    open System.Collections
    open System.Collections.Generic
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections

    open SeqComposer.Microsoft.FSharp.Patches

    module internal IEnumerator =

      let noReset() = raise (new System.NotSupportedException(SR.GetString(SR.resetNotSupported)))
      let notStarted() = raise (new System.InvalidOperationException(SR.GetString(SR.enumerationNotStarted)))
      let alreadyFinished() = raise (new System.InvalidOperationException(SR.GetString(SR.enumerationAlreadyFinished)))
      let check started = if not started then notStarted()
      let dispose (r : System.IDisposable) = r.Dispose()

      let cast (e : IEnumerator) : IEnumerator<'T> =
          { new IEnumerator<'T> with
                member x.Current = unbox<'T> e.Current
            interface IEnumerator with
                member x.Current = unbox<'T> e.Current :> obj
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = noReset()
            interface System.IDisposable with
                member x.Dispose() =
                    match e with
                    | :? System.IDisposable as e -> e.Dispose()
                    | _ -> ()   }

      /// A concrete implementation of an enumerator that returns no values
      [<Sealed>]
      type EmptyEnumerator<'T>() =
          let mutable started = false
          interface IEnumerator<'T> with
                member x.Current =
                  check started
                  (alreadyFinished() : 'T)

          interface System.Collections.IEnumerator with
              member x.Current =
                  check started
                  (alreadyFinished() : obj)
              member x.MoveNext() =
                  if not started then started <- true
                  false
              member x.Reset() = noReset()
          interface System.IDisposable with
                member x.Dispose() = ()
                
      let Empty<'T> () = (new EmptyEnumerator<'T>() :> IEnumerator<'T>)
      [<NoEquality; NoComparison>]
      type EmptyEnumerable<'T> =
            | EmptyEnumerable
            interface IEnumerable<'T> with
                member x.GetEnumerator() = Empty<'T>()
            interface IEnumerable with
                member x.GetEnumerator() = (Empty<'T>() :> IEnumerator)

      let readAndClear r =
          lock r (fun () -> match !r with None -> None | Some _ as res -> r := None; res)

      let generateWhileSome openf compute closef : IEnumerator<'U> =
          let started = ref false
          let curr = ref None
          let state = ref (Some(openf()))
          let getCurr() =
              check !started
              match !curr with None -> alreadyFinished() | Some x -> x
          let start() = if not !started then (started := true)

          let dispose() = readAndClear state |> Option.iter closef
          let finish() = (try dispose() finally curr := None)
          {  new IEnumerator<'U> with
                 member x.Current = getCurr()
             interface IEnumerator with
                 member x.Current = box (getCurr())
                 member x.MoveNext() =
                     start()
                     match !state with
                     | None -> false (* we started, then reached the end, then got another MoveNext *)
                     | Some s ->
                         match (try compute s with e -> finish(); reraise()) with
                         | None -> finish(); false
                         | Some _ as x -> curr := x; true

                 member x.Reset() = noReset()
             interface System.IDisposable with
                 member x.Dispose() = dispose() }

      [<Sealed>]
      type Singleton<'T>(v:'T) =
          let mutable started = false
          interface IEnumerator<'T> with
                member x.Current = v
          interface IEnumerator with
              member x.Current = box v
              member x.MoveNext() = if started then false else (started <- true; true)
              member x.Reset() = noReset()
          interface System.IDisposable with
              member x.Dispose() = ()

      let Singleton x = (new Singleton<'T>(x) :> IEnumerator<'T>)

      let EnumerateThenFinally f (e : IEnumerator<'T>) =
          { new IEnumerator<'T> with
                member x.Current = e.Current
            interface IEnumerator with
                member x.Current = (e :> IEnumerator).Current
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = noReset()
            interface System.IDisposable with
                member x.Dispose() =
                    try
                        e.Dispose()
                    finally
                        f()
          }
    
      let inline checkNonNull argName arg =
        match box arg with
        | null -> nullArg argName
        | _ -> ()

      let mkSeq f =
          { new IEnumerable<'U> with
                member x.GetEnumerator() = f()
            interface IEnumerable with
                member x.GetEnumerator() = (f() :> IEnumerator) 
          }

namespace SeqComposer.Microsoft.FSharp.Core.CompilerServices

    open System
    open System.Diagnostics
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Primitives.Basics
    open System.Collections
    open System.Collections.Generic
    
    open SeqComposer.Microsoft.FSharp.Patches
    open SeqComposer.Microsoft.FSharp.Collections
    open SeqComposer.Microsoft.FSharp.Collections.IEnumerator
     
    module RuntimeHelpers =

        [<Struct; NoComparison; NoEquality>]
        type internal StructBox<'T when 'T : equality>(value:'T) =
            member x.Value = value
            static member Comparer =
                let gcomparer = HashIdentity.Structural<'T>
                { new IEqualityComparer<StructBox<'T>> with
                       member __.GetHashCode(v) = gcomparer.GetHashCode(v.Value)
                       member __.Equals(v1,v2) = gcomparer.Equals(v1.Value,v2.Value) }

        let Generate openf compute closef =
            mkSeq (fun () -> IEnumerator.generateWhileSome openf compute closef)

        let GenerateUsing (openf : unit -> ('U :> System.IDisposable)) compute =
            Generate openf compute (fun (s:'U) -> s.Dispose())

        let EnumerateFromFunctions opener moveNext current =
            Generate
                opener
                (fun x -> if moveNext x then Some(current x) else None)
                (fun x -> match box(x) with :? System.IDisposable as id -> id.Dispose() | _ -> ())

        // A family of enumerators that can have additional 'finally' actions added to the enumerator through
        // the use of mutation. This is used to 'push' the disposal action for a 'use' into the next enumerator.
        // For example,
        //    seq { use x = ...
        //          while ... }
        // results in the 'while' loop giving an adjustable enumerator. This is then adjusted by adding the disposal action
        // from the 'use' into the enumerator. This means that we avoid constructing a two-deep enumerator chain in this
        // common case.
        type IFinallyEnumerator =
            abstract AppendFinallyAction : (unit -> unit) -> unit

        /// A concrete implementation of IEnumerable that adds the given compensation to the "Dispose" chain of any
        /// enumerators returned by the enumerable.
        [<Sealed>]
        type FinallyEnumerable<'T>(compensation: unit -> unit, restf: unit -> seq<'T>) =
            interface IEnumerable<'T> with
                member x.GetEnumerator() =
                    try
                        let ie = restf().GetEnumerator()
                        match ie with
                        | :? IFinallyEnumerator as a ->
                            a.AppendFinallyAction(compensation)
                            ie
                        | _ ->
                            IEnumerator.EnumerateThenFinally compensation ie
                    with e ->
                        compensation()
                        reraise()
            interface IEnumerable with
                member x.GetEnumerator() = ((x :> IEnumerable<'T>).GetEnumerator() :> IEnumerator)

        /// An optimized object for concatenating a sequence of enumerables
        [<Sealed>]
        type ConcatEnumerator<'T,'U when 'U :> seq<'T>>(sources: seq<'U>) =
            let mutable outerEnum = sources.GetEnumerator()
            let mutable currInnerEnum = IEnumerator.Empty()

            let mutable started = false
            let mutable finished = false
            let mutable compensations = []

            [<DefaultValue(false)>] // false = unchecked
            val mutable private currElement : 'T

            member x.Finish() =
                finished <- true
                try
                    match currInnerEnum with
                    | null -> ()
                    | _ ->
                        try
                            currInnerEnum.Dispose()
                        finally
                            currInnerEnum <- null
                finally
                    try
                        match outerEnum with
                        | null -> ()
                        | _ ->
                            try
                                outerEnum.Dispose()
                            finally
                                outerEnum <- null
                    finally
                        let rec iter comps =
                            match comps with
                            |   [] -> ()
                            |   h::t ->
                                    try h() finally iter t
                        try
                            compensations |> List.rev |> iter
                        finally
                            compensations <- []

            member x.GetCurrent() =
                IEnumerator.check started
                if finished then IEnumerator.alreadyFinished() else x.currElement

            interface IFinallyEnumerator with
                member x.AppendFinallyAction(f) =
                    compensations <- f :: compensations

            interface IEnumerator<'T> with
                member x.Current = x.GetCurrent()

            interface IEnumerator with
                member x.Current = box (x.GetCurrent())

                member x.MoveNext() =
                   if not started then (started <- true)
                   if finished then false
                   else
                      let rec takeInner () =
                        // check the inner list
                        if currInnerEnum.MoveNext() then
                            x.currElement <- currInnerEnum.Current
                            true
                        else
                            // check the outer list
                            let rec takeOuter() =
                                if outerEnum.MoveNext() then
                                    let ie = outerEnum.Current
                                    // Optimization to detect the statically-allocated empty IEnumerables
                                    match box ie with
                                    | :? EmptyEnumerable<'T> ->
                                         // This one is empty, just skip, don't call GetEnumerator, try again
                                         takeOuter()
                                    | _ ->
                                         // OK, this one may not be empty.
                                         // Don't forget to dispose of the enumerator for the inner list now we're done with it
                                         currInnerEnum.Dispose()
                                         currInnerEnum <- ie.GetEnumerator()
                                         takeInner ()
                                else
                                    // We're done
                                    x.Finish()
                                    false
                            takeOuter()
                      takeInner ()

                member x.Reset() = IEnumerator.noReset()

            interface System.IDisposable with
                member x.Dispose() =
                    if not finished then
                        x.Finish()

        let EnumerateUsing (resource : 'T :> System.IDisposable) (rest: 'T -> #seq<'U>) =
            (FinallyEnumerable((fun () -> match box resource with null -> () | _ -> resource.Dispose()),
                               (fun () -> rest resource :> seq<_>)) :> seq<_>)

        let mkConcatSeq (sources: seq<'U :> seq<'T>>) =
            mkSeq (fun () -> new ConcatEnumerator<_,_>(sources) :> IEnumerator<'T>)

        let EnumerateWhile (g : unit -> bool) (b: seq<'T>) : seq<'T> =
            let started = ref false
            let curr = ref None
            let getCurr() =
                IEnumerator.check !started
                match !curr with None -> IEnumerator.alreadyFinished() | Some x -> x
            let start() = if not !started then (started := true)

            let finish() = (curr := None)
            mkConcatSeq
               (mkSeq (fun () ->
                    { new IEnumerator<_> with
                          member x.Current = getCurr()
                       interface IEnumerator with
                          member x.Current = box (getCurr())
                          member x.MoveNext() =
                               start()
                               let keepGoing = (try g() with e -> finish (); reraise ()) in
                               if keepGoing then
                                   curr := Some(b); true
                               else
                                   finish(); false
                          member x.Reset() = IEnumerator.noReset()
                       interface System.IDisposable with
                          member x.Dispose() = () }))

        let EnumerateThenFinally (rest : seq<'T>) (compensation : unit -> unit)  =
            (FinallyEnumerable(compensation, (fun () -> rest)) :> seq<_>)

        let CreateEvent (add : 'Delegate -> unit) (remove : 'Delegate -> unit) (create : (obj -> 'Args -> unit) -> 'Delegate ) :IEvent<'Delegate,'Args> =
            // Note, we implement each interface explicitly: this works around a bug in the CLR
            // implementation on CompactFramework 3.7, used on Windows Phone 7
            { new obj() with
                  member x.ToString() = "<published event>"
              interface IEvent<'Delegate,'Args>
              interface IDelegateEvent<'Delegate> with
                 member x.AddHandler(h) = add h
                 member x.RemoveHandler(h) = remove h
              interface System.IObservable<'Args> with
                 member x.Subscribe(r:IObserver<'Args>) =
                     let h = create (fun _ args -> r.OnNext(args))
                     add h
                     { new System.IDisposable with
                          member x.Dispose() = remove h } }


    [<AbstractClass>]
    type GeneratedSequenceBase<'T>() =
        let mutable redirectTo : GeneratedSequenceBase<'T> = Unchecked.defaultof<_>
        let mutable redirect : bool = false

        abstract GetFreshEnumerator : unit -> IEnumerator<'T>
        abstract GenerateNext : next:byref<IEnumerable<'T>> -> int // 0 = Stop, 1 = Yield, 2 = Goto
        abstract Close: unit -> unit
        abstract CheckClose: bool
        abstract LastGenerated : 'T

        //[<System.Diagnostics.DebuggerNonUserCode; System.Diagnostics.DebuggerStepThroughAttribute>]
        member x.MoveNextImpl() =
             let active =
                 if redirect then redirectTo
                 else x
             let mutable target = null
             match active.GenerateNext(&target) with
             | 1 ->
                 true
             | 2 ->
                 match target.GetEnumerator() with
                 | :? GeneratedSequenceBase<'T> as g when not active.CheckClose ->
                     redirectTo <- g
                 | e ->
                     redirectTo <-
                           { new GeneratedSequenceBase<'T>() with
                                 member x.GetFreshEnumerator() = e
                                 member x.GenerateNext(_) = if e.MoveNext() then 1 else 0
                                 member x.Close() = try e.Dispose() finally active.Close()
                                 member x.CheckClose = true
                                 member x.LastGenerated = e.Current }
                 redirect <- true
                 x.MoveNextImpl()
             | _ (* 0 *)  ->
                 false

        interface IEnumerable<'T> with
            member x.GetEnumerator() = x.GetFreshEnumerator()
        interface IEnumerable with
            member x.GetEnumerator() = (x.GetFreshEnumerator() :> IEnumerator)
        interface IEnumerator<'T> with
            member x.Current = if redirect then redirectTo.LastGenerated else x.LastGenerated
            member x.Dispose() = if redirect then redirectTo.Close() else x.Close()
        interface IEnumerator with
            member x.Current = box (if redirect then redirectTo.LastGenerated else x.LastGenerated)

            //[<System.Diagnostics.DebuggerNonUserCode; System.Diagnostics.DebuggerStepThroughAttribute>]
            member x.MoveNext() = x.MoveNextImpl()

            member x.Reset() = raise <| new System.NotSupportedException()

// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace SeqComposer.Microsoft.FSharp.Collections

    open System
    open System.Diagnostics
    open System.Collections
    open System.Collections.Generic
    open System.Reflection
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics
    
    open SeqComposer.Microsoft.FSharp.Patches

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Composer =
        open IEnumerator

        module Core =
            [<Struct; NoComparison; NoEquality>]
            type NoValue = struct end

            [<Struct; NoComparison; NoEquality>]
            type Values<'a,'b> =
                val mutable _1 : 'a
                val mutable _2 : 'b

                new (a:'a, b: 'b) = { _1 = a;  _2 = b }

            [<Struct; NoComparison; NoEquality>]
            type Values<'a,'b,'c> =
                val mutable _1 : 'a
                val mutable _2 : 'b
                val mutable _3 : 'c

                new (a:'a, b:'b, c:'c) = { _1 = a; _2 = b; _3 = c }

            type PipeIdx = int

            type IOutOfBand =
                abstract StopFurtherProcessing : PipeIdx -> unit

            [<AbstractClass>]
            type Consumer() =
                abstract ChainComplete : stopTailCall:byref<unit> * PipeIdx -> unit
                abstract ChainDispose  : stopTailCall:byref<unit> -> unit

            [<AbstractClass>]
            type Consumer<'T,'U> () =
                inherit Consumer()

                abstract ProcessNext : input:'T -> bool

                override this.ChainComplete (_,_) = ()
                override this.ChainDispose _ = ()

            [<AbstractClass>]
            type ConsumerWithState<'T,'U,'State> =
                inherit Consumer<'T,'U>

                val mutable State : 'State

                new (initState) = {
                    State = initState
                }

            [<AbstractClass>]
            type ConsumerChainedWithState<'T,'U,'State> =
                inherit ConsumerWithState<'T,'U,'State>

                val private Next : Consumer

                new (next:Consumer, initState) = {
                    inherit ConsumerWithState<'T,'U,'State> (initState)
                    Next = next
                }

                override this.ChainComplete (stopTailCall, terminatingIdx) =
                    this.Next.ChainComplete (&stopTailCall, terminatingIdx)
                override this.ChainDispose stopTailCall =
                    this.Next.ChainDispose (&stopTailCall)

            [<AbstractClass>]
            type ConsumerChained<'T,'U>(next:Consumer) =
                inherit ConsumerChainedWithState<'T,'U,NoValue>(next, Unchecked.defaultof<NoValue>)

            [<AbstractClass>]
            type ConsumerChainedWithStateAndCleanup<'T,'U,'State> (next, inititalState) =
                inherit ConsumerChainedWithState<'T,'U,'State>(next, inititalState)

                abstract OnComplete : PipeIdx -> unit
                abstract OnDispose  : unit -> unit

                override this.ChainComplete (stopTailCall, terminatingIdx) =
                    this.OnComplete terminatingIdx
                    next.ChainComplete (&stopTailCall, terminatingIdx)
                override this.ChainDispose stopTailCall  =
                    try     this.OnDispose ()
                    finally next.ChainDispose (&stopTailCall)

            [<AbstractClass>]
            type ConsumerChainedWithCleanup<'T,'U>(next:Consumer) =
                inherit ConsumerChainedWithStateAndCleanup<'T,'U,NoValue>(next, Unchecked.defaultof<NoValue>)

            [<AbstractClass>]
            type Folder<'T,'Result,'State> =
                inherit ConsumerWithState<'T,'T,'State>

                val mutable Result : 'Result
                val mutable HaltedIdx : int

                member this.StopFurtherProcessing pipeIdx = this.HaltedIdx <- pipeIdx
                interface IOutOfBand with
                    member this.StopFurtherProcessing pipeIdx = this.StopFurtherProcessing pipeIdx

                new (initalResult,initState) = {
                    inherit ConsumerWithState<'T,'T,'State>(initState)
                    HaltedIdx = 0
                    Result = initalResult
                }

            [<AbstractClass>]
            type Folder<'T,'Result>(initResult) =
                inherit Folder<'T,'Result,NoValue>(initResult,Unchecked.defaultof<NoValue>)

            [<AbstractClass>]
            type FolderWithCleanup<'T,'Result,'State>(initResult,initState) =
                inherit Folder<'T,'Result,'State>(initResult,initState)

                abstract OnComplete : PipeIdx -> unit
                abstract OnDispose : unit -> unit

                override this.ChainComplete (stopTailCall, terminatingIdx) =
                    this.OnComplete terminatingIdx
                override this.ChainDispose _ =
                    this.OnDispose ()

            [<AbstractClass>]
            type SeqFactory<'T,'U> () =
                abstract PipeIdx : PipeIdx
                abstract Create<'V> : IOutOfBand -> PipeIdx -> Consumer<'U,'V> -> Consumer<'T,'V>

                default __.PipeIdx = 1

                member this.Build outOfBand next = this.Create outOfBand 1 next

            type ISeq<'T> =
                inherit IEnumerable<'T>
                abstract member Compose<'U> : (SeqFactory<'T,'U>) -> ISeq<'U>
                abstract member ForEach<'Result,'State> : f:(PipeIdx->Folder<'T,'Result,'State>) -> 'Result

        open Core

        module internal TailCall =
            // used for performance reasons; these are not recursive calls, so should be safe
            // ** it should be noted that potential changes to the f# compiler may render this function
            // ineffictive **
            let inline avoid boolean = match boolean with true -> true | false -> false

        module internal Upcast =
            // The f# compiler outputs unnecessary unbox.any calls in upcasts. If this functionality
            // is fixed with the compiler then these functions can be removed.
            let inline seq (t:#ISeq<'T>) : ISeq<'T> = (# "" t : ISeq<'T> #)
            let inline enumerable (t:#IEnumerable<'T>) : IEnumerable<'T> = (# "" t : IEnumerable<'T> #)
            let inline enumerator (t:#IEnumerator<'T>) : IEnumerator<'T> = (# "" t : IEnumerator<'T> #)
            let inline enumeratorNonGeneric (t:#IEnumerator) : IEnumerator = (# "" t : IEnumerator #)
            let inline outOfBand (t:#IOutOfBand) : IOutOfBand = (# "" t : IOutOfBand #)

        type ComposedFactory<'T,'U,'V> private (first:SeqFactory<'T,'U>, second:SeqFactory<'U,'V>, secondPipeIdx:PipeIdx) =
            inherit SeqFactory<'T,'V>()

            override __.PipeIdx =
                secondPipeIdx

            override this.Create<'W> (outOfBand:IOutOfBand) (pipeIdx:PipeIdx) (next:Consumer<'V,'W>) : Consumer<'T,'W> =
                first.Create outOfBand pipeIdx (second.Create outOfBand secondPipeIdx next)

            static member Combine (first:SeqFactory<'T,'U>) (second:SeqFactory<'U,'V>) : SeqFactory<'T,'V> =
                upcast ComposedFactory(first, second, first.PipeIdx+1)

        and IdentityFactory<'T> () =
            inherit SeqFactory<'T,'T> ()
            static let singleton : SeqFactory<'T,'T> = upcast (IdentityFactory<'T>())
            override __.Create<'V> (_outOfBand:IOutOfBand) (_pipeIdx:PipeIdx) (next:Consumer<'T,'V>) : Consumer<'T,'V> = next
            static member Instance = singleton

        and ISkipping =
            // Seq.init(Infinite)? lazily uses Current. The only Composer component that can do that is Skip
            // and it can only do it at the start of a sequence
            abstract Skipping : unit -> bool

        type SeqProcessNextStates =
        | InProcess  = 0
        | NotStarted = 1
        | Finished   = 2

        type Result<'T>() =
            inherit Folder<'T,'T>(Unchecked.defaultof<'T>)

            member val SeqState = SeqProcessNextStates.NotStarted with get, set

            override this.ProcessNext (input:'T) : bool =
                this.Result <- input
                true

        module ForEach =
            type IIterate<'T> =
                abstract Iterate<'U,'Result,'State> : outOfBand:Folder<'U,'Result,'State> -> consumer:Consumer<'T,'U> -> unit

            [<Struct;NoComparison;NoEquality>]
            type enumerable<'T> (enumerable:IEnumerable<'T>) =
                interface IIterate<'T> with
                    member __.Iterate (outOfBand:Folder<'U,'Result,'State>) (consumer:Consumer<'T,'U>) =
                        use enumerator = enumerable.GetEnumerator ()
                        let rec iterate () =
                            if enumerator.MoveNext () then  
                                consumer.ProcessNext enumerator.Current |> ignore
                                if outOfBand.HaltedIdx = 0 then
                                    iterate ()
                        iterate ()

            [<Struct;NoComparison;NoEquality>]
            type Array<'T> (array:array<'T>) =
                interface IIterate<'T> with
                    member __.Iterate (outOfBand:Folder<'U,'Result,'State>) (consumer:Consumer<'T,'U>) =
                        let mutable idx = 0
                        while (outOfBand.HaltedIdx = 0) && (idx < array.Length) do
                            consumer.ProcessNext array.[idx] |> ignore
                            idx <- idx + 1

            [<Struct;NoComparison;NoEquality>]
            type List<'T> (alist:list<'T>) =
                interface IIterate<'T> with
                    member __.Iterate (outOfBand:Folder<'U,'Result,'State>) (consumer:Consumer<'T,'U>) =
                        let rec iterate lst =
                            match outOfBand.HaltedIdx, lst with
                            | 0, hd :: tl ->
                                consumer.ProcessNext hd |> ignore
                                iterate tl
                            | _ -> ()
                        iterate alist

            type unfold<'S,'T> (generator:'S->option<'T*'S>, state:'S) =
                interface IIterate<'T> with
                    member __.Iterate (outOfBand:Folder<'U,'Result,'State>) (consumer:Consumer<'T,'U>) =
                        let rec iterate current =
                            match outOfBand.HaltedIdx, generator current with
                            | 0, Some (item, next) ->
                                consumer.ProcessNext item |> ignore
                                iterate next
                            | _ -> ()

                        iterate state

            let makeIsSkipping (consumer:Consumer<'T,'U>) =
                match box consumer with
                | :? ISkipping as skip -> skip.Skipping
                | _ -> fun () -> false

            type init<'T> (f, terminatingIdx:int) =
                interface IIterate<'T> with
                    member __.Iterate (outOfBand:Folder<'U,'Result,'State>) (consumer:Consumer<'T,'U>) =
                        let mutable idx = -1
                        let isSkipping = makeIsSkipping consumer
                        let mutable maybeSkipping = true
                        while (outOfBand.HaltedIdx = 0) && (idx < terminatingIdx) do
                            if maybeSkipping then
                                maybeSkipping <- isSkipping ()

                            if not maybeSkipping then
                                consumer.ProcessNext (f (idx+1)) |> ignore

                            idx <- idx + 1

            let execute (f:PipeIdx->Folder<'U,'Result,'State>) (current:SeqFactory<'T,'U>) (executeOn:#IIterate<'T>) =
                let mutable stopTailCall = ()
                let result = f (current.PipeIdx+1)
                let consumer = current.Build (Upcast.outOfBand result) result
                try
                    executeOn.Iterate result consumer
                    consumer.ChainComplete (&stopTailCall, result.HaltedIdx)
                    result.Result
                finally
                    consumer.ChainDispose (&stopTailCall)

            let executeThin (f:PipeIdx->Folder<'T,'Result,'State>) (executeOn:#IIterate<'T>) =
                let mutable stopTailCall = ()
                let result = f 1
                try
                    executeOn.Iterate result result
                    result.ChainComplete (&stopTailCall, result.HaltedIdx)
                    result.Result
                finally
                    result.ChainDispose (&stopTailCall)

        module Enumerable =
            type Empty<'T>() =
                let current () = failwith "library implementation error: Current should never be called"
                interface IEnumerator<'T> with
                    member __.Current = current ()
                interface IEnumerator with
                    member __.Current = current ()
                    member __.MoveNext () = false
                    member __.Reset (): unit = noReset ()
                interface IDisposable with
                    member __.Dispose () = ()

            type EmptyEnumerators<'T>() =
                static let element : IEnumerator<'T> = upcast (new Empty<'T> ())
                static member Element = element

            [<AbstractClass>]
            type EnumeratorBase<'T>(result:Result<'T>, seqComponent:Consumer) =
                interface IDisposable with
                    member __.Dispose() : unit =
                        let mutable stopTailCall = ()
                        seqComponent.ChainDispose (&stopTailCall)

                interface IEnumerator with
                    member this.Current : obj = box ((Upcast.enumerator this)).Current
                    member __.MoveNext () = failwith "library implementation error: derived class should implement (should be abstract)"
                    member __.Reset () : unit = noReset ()

                interface IEnumerator<'T> with
                    member __.Current =
                        if result.SeqState = SeqProcessNextStates.InProcess then result.Result
                        else
                            match result.SeqState with
                            | SeqProcessNextStates.NotStarted -> notStarted()
                            | SeqProcessNextStates.Finished -> alreadyFinished()
                            | _ -> failwith "library implementation error: all states should have been handled"

            and [<AbstractClass>] EnumerableBase<'T> () =
                let derivedClassShouldImplement () =
                    failwith "library implementation error: derived class should implement (should be abstract)"

                abstract member Append   : (seq<'T>) -> IEnumerable<'T>

                default this.Append source = Upcast.enumerable (AppendEnumerable [this; source])

                interface IEnumerable with
                    member this.GetEnumerator () : IEnumerator =
                        let genericEnumerable = Upcast.enumerable this
                        let genericEnumerator = genericEnumerable.GetEnumerator ()
                        Upcast.enumeratorNonGeneric genericEnumerator

                interface IEnumerable<'T> with
                    member this.GetEnumerator () : IEnumerator<'T> = derivedClassShouldImplement ()

                interface ISeq<'T> with
                    member __.Compose _ = derivedClassShouldImplement ()
                    member __.ForEach _ = derivedClassShouldImplement ()

            and Enumerator<'T,'U>(source:IEnumerator<'T>, seqComponent:Consumer<'T,'U>, result:Result<'U>) =
                inherit EnumeratorBase<'U>(result, seqComponent)

                let rec moveNext () =
                    if (result.HaltedIdx = 0) && source.MoveNext () then
                        if seqComponent.ProcessNext source.Current then
                            true
                        else
                            moveNext ()
                    else
                        result.SeqState <- SeqProcessNextStates.Finished
                        let mutable stopTailCall = ()
                        (seqComponent).ChainComplete (&stopTailCall, result.HaltedIdx)
                        false

                interface IEnumerator with
                    member __.MoveNext () =
                        result.SeqState <- SeqProcessNextStates.InProcess
                        moveNext ()

                interface IDisposable with
                    member __.Dispose() =
                        try
                            source.Dispose ()
                        finally
                            let mutable stopTailCall = ()
                            (seqComponent).ChainDispose (&stopTailCall)

            and Enumerable<'T,'U>(enumerable:IEnumerable<'T>, current:SeqFactory<'T,'U>) =
                inherit EnumerableBase<'U>()

                interface IEnumerable<'U> with
                    member this.GetEnumerator () : IEnumerator<'U> =
                        let result = Result<'U> ()
                        Upcast.enumerator (new Enumerator<'T,'U>(enumerable.GetEnumerator(), current.Build (Upcast.outOfBand result) result, result))

                interface ISeq<'U> with
                    member __.Compose (next:SeqFactory<'U,'V>) : ISeq<'V> =
                        Upcast.seq (new Enumerable<'T,'V>(enumerable, ComposedFactory.Combine current next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'U,'Result,'State>) =
                        ForEach.execute f current (ForEach.enumerable enumerable)

            and EnumerableThin<'T>(enumerable:IEnumerable<'T>) =
                inherit EnumerableBase<'T>()

                interface IEnumerable<'T> with
                    member this.GetEnumerator () = enumerable.GetEnumerator ()

                interface ISeq<'T> with
                    member __.Compose (next:SeqFactory<'T,'U>) : ISeq<'U> =
                        Upcast.seq (new Enumerable<'T,'U>(enumerable, next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'T,'Result,'State>) =
                        ForEach.executeThin f (ForEach.enumerable enumerable)

            and ConcatEnumerator<'T, 'Collection when 'Collection :> seq<'T>> (sources:seq<'Collection>) =
                let mutable state = SeqProcessNextStates.NotStarted
                let main = sources.GetEnumerator ()

                let mutable active = EmptyEnumerators.Element

                let rec moveNext () =
                    if active.MoveNext () then
                        true
                    elif main.MoveNext () then
                        active.Dispose ()
                        active <- main.Current.GetEnumerator ()
                        moveNext ()
                    else
                        state <- SeqProcessNextStates.Finished
                        false

                interface IEnumerator<'T> with
                    member __.Current =
                        if state = SeqProcessNextStates.InProcess then active.Current
                        else
                            match state with
                            | SeqProcessNextStates.NotStarted -> notStarted()
                            | SeqProcessNextStates.Finished -> alreadyFinished()
                            | _ -> failwith "library implementation error: all states should have been handled"

                interface IEnumerator with
                    member this.Current = box ((Upcast.enumerator this)).Current
                    member __.MoveNext () =
                        state <- SeqProcessNextStates.InProcess
                        moveNext ()
                    member __.Reset () = noReset ()

                interface IDisposable with
                    member __.Dispose() =
                        main.Dispose ()
                        active.Dispose ()

            and AppendEnumerable<'T> (sources:list<seq<'T>>) =
                inherit EnumerableBase<'T>()

                interface IEnumerable<'T> with
                    member this.GetEnumerator () : IEnumerator<'T> =
                        Upcast.enumerator (new ConcatEnumerator<_,_> (sources |> List.rev))

                override this.Append source =
                    Upcast.enumerable (AppendEnumerable (source :: sources))

                interface ISeq<'T> with
                    member this.Compose (next:SeqFactory<'T,'U>) : ISeq<'U> =
                        Upcast.seq (Enumerable<'T,'V>(this, next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'T,'Result,'State>) =
                        ForEach.executeThin f (ForEach.enumerable this)

            and ConcatEnumerable<'T, 'Collection when 'Collection :> seq<'T>> (sources:seq<'Collection>) =
                inherit EnumerableBase<'T>()

                interface IEnumerable<'T> with
                    member this.GetEnumerator () : IEnumerator<'T> =
                        Upcast.enumerator (new ConcatEnumerator<_,_> (sources))

                interface ISeq<'T> with
                    member this.Compose (next:SeqFactory<'T,'U>) : ISeq<'U> =
                        Upcast.seq (Enumerable<'T,'V>(this, next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'T,'Result,'State>) =
                        ForEach.executeThin f (ForEach.enumerable this)

            let create enumerable current =
                Upcast.seq (Enumerable(enumerable, current))

        module EmptyEnumerable =
            type Enumerable<'T> () =
                inherit Enumerable.EnumerableBase<'T>()

                static let singleton = Enumerable<'T>() :> ISeq<'T>
                static member Instance = singleton

                interface IEnumerable<'T> with
                    member this.GetEnumerator () : IEnumerator<'T> = IEnumerator.Empty<'T>()

                override this.Append source =
                    Upcast.enumerable (Enumerable.EnumerableThin<'T> source)

                interface ISeq<'T> with
                    member this.Compose (next:SeqFactory<'T,'U>) : ISeq<'U> =
                        Upcast.seq (Enumerable.Enumerable<'T,'V>(this, next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'T,'Result,'State>) =
                        ForEach.executeThin f (ForEach.enumerable this)



        module Array =
            type Enumerator<'T,'U>(delayedArray:unit->array<'T>, seqComponent:Consumer<'T,'U>, result:Result<'U>) =
                inherit Enumerable.EnumeratorBase<'U>(result, seqComponent)

                let mutable idx = 0
                let mutable array = Unchecked.defaultof<_>

                let mutable initMoveNext = Unchecked.defaultof<_>
                do
                    initMoveNext <-
                        fun () ->
                            result.SeqState <- SeqProcessNextStates.InProcess
                            array <- delayedArray ()
                            initMoveNext <- ignore

                let rec moveNext () =
                    if (result.HaltedIdx = 0) && idx < array.Length then
                        idx <- idx+1
                        if seqComponent.ProcessNext array.[idx-1] then
                            true
                        else
                            moveNext ()
                    else
                        result.SeqState <- SeqProcessNextStates.Finished
                        let mutable stopTailCall = ()
                        (seqComponent).ChainComplete (&stopTailCall, result.HaltedIdx)
                        false

                interface IEnumerator with
                    member __.MoveNext () =
                        initMoveNext ()
                        moveNext ()

            type Enumerable<'T,'U>(delayedArray:unit->array<'T>, current:SeqFactory<'T,'U>) =
                inherit Enumerable.EnumerableBase<'U>()

                interface IEnumerable<'U> with
                    member this.GetEnumerator () : IEnumerator<'U> =
                        let result = Result<'U> ()
                        Upcast.enumerator (new Enumerator<'T,'U>(delayedArray, current.Build (Upcast.outOfBand result) result, result))

                interface ISeq<'U> with
                    member __.Compose (next:SeqFactory<'U,'V>) : ISeq<'V> =
                        Upcast.seq (new Enumerable<'T,'V>(delayedArray, ComposedFactory.Combine current next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'U,'Result,'State>) =
                        ForEach.execute f current (ForEach.Array (delayedArray ()))

            let createDelayed (delayedArray:unit->array<'T>) (current:SeqFactory<'T,'U>) =
                Upcast.seq (Enumerable(delayedArray, current))

            let create (array:array<'T>) (current:SeqFactory<'T,'U>) =
                createDelayed (fun () -> array) current

            let createDelayedId (delayedArray:unit -> array<'T>) =
                createDelayed delayedArray IdentityFactory.Instance

            let createId (array:array<'T>) =
                create array IdentityFactory.Instance

        module List =
            type Enumerator<'T,'U>(alist:list<'T>, seqComponent:Consumer<'T,'U>, result:Result<'U>) =
                inherit Enumerable.EnumeratorBase<'U>(result, seqComponent)

                let mutable list = alist

                let rec moveNext current =
                    match result.HaltedIdx, current with
                    | 0, head::tail ->
                        if seqComponent.ProcessNext head then
                            list <- tail
                            true
                        else
                            moveNext tail
                    | _ ->
                        result.SeqState <- SeqProcessNextStates.Finished
                        let mutable stopTailCall = ()
                        (seqComponent).ChainComplete (&stopTailCall, result.HaltedIdx)
                        false

                interface IEnumerator with
                    member __.MoveNext () =
                        result.SeqState <- SeqProcessNextStates.InProcess
                        moveNext list

            type Enumerable<'T,'U>(alist:list<'T>, current:SeqFactory<'T,'U>) =
                inherit Enumerable.EnumerableBase<'U>()

                interface IEnumerable<'U> with
                    member this.GetEnumerator () : IEnumerator<'U> =
                        let result = Result<'U> ()
                        Upcast.enumerator (new Enumerator<'T,'U>(alist, current.Build (Upcast.outOfBand result) result, result))

                interface ISeq<'U> with
                    member __.Compose (next:SeqFactory<'U,'V>) : ISeq<'V> =
                        Upcast.seq (new Enumerable<'T,'V>(alist, ComposedFactory.Combine current next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'U,'Result,'State>) =
                        ForEach.execute f current (ForEach.List alist)

            let create alist current =
                Upcast.seq (Enumerable(alist, current))

        module Unfold =
            type Enumerator<'T,'U,'State>(generator:'State->option<'T*'State>, state:'State, seqComponent:Consumer<'T,'U>, result:Result<'U>) =
                inherit Enumerable.EnumeratorBase<'U>(result, seqComponent)

                let mutable current = state

                let rec moveNext () =
                    match result.HaltedIdx, generator current with
                    | 0, Some (item, nextState) ->
                        current <- nextState
                        if seqComponent.ProcessNext item then
                            true
                        else
                            moveNext ()
                    | _ -> false

                interface IEnumerator with
                    member __.MoveNext () =
                        result.SeqState <- SeqProcessNextStates.InProcess
                        moveNext ()

            type Enumerable<'T,'U,'GeneratorState>(generator:'GeneratorState->option<'T*'GeneratorState>, state:'GeneratorState, current:SeqFactory<'T,'U>) =
                inherit Enumerable.EnumerableBase<'U>()

                interface IEnumerable<'U> with
                    member this.GetEnumerator () : IEnumerator<'U> =
                        let result = Result<'U> ()
                        Upcast.enumerator (new Enumerator<'T,'U,'GeneratorState>(generator, state, current.Build (Upcast.outOfBand result) result, result))

                interface ISeq<'U> with
                    member this.Compose (next:SeqFactory<'U,'V>) : ISeq<'V> =
                        Upcast.seq (new Enumerable<'T,'V,'GeneratorState>(generator, state, ComposedFactory.Combine current next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'U,'Result,'State>) =
                        ForEach.execute f current (ForEach.unfold (generator, state))

        module Init =
            // The original implementation of "init" delayed the calculation of Current, and so it was possible
            // to do MoveNext without it's value being calculated.
            // I can imagine only two scenerios where that is possibly sane, although a simple solution is readily
            // at hand in both cases. The first is that of an expensive generator function, where you skip the
            // first n elements. The simple solution would have just been to have a map ((+) n) as the first operation
            // instead. The second case would be counting elements, but that is only of use if you're not filtering
            // or mapping or doing anything else (as that would cause Current to be evaluated!) and
            // so you already know what the count is!! Anyway, someone thought it was a good idea, so
            // I have had to add an extra function that is used in Skip to determine if we are touching
            // Current or not.

            let getTerminatingIdx (count:Nullable<int>) =
                // we are offset by 1 to allow for values going up to System.Int32.MaxValue
                // System.Int32.MaxValue is an illegal value for the "infinite" sequence
                if count.HasValue then
                    count.Value - 1
                else
                    System.Int32.MaxValue

            type Enumerator<'T,'U>(count:Nullable<int>, f:int->'T, seqComponent:Consumer<'T,'U>, result:Result<'U>) =
                inherit Enumerable.EnumeratorBase<'U>(result, seqComponent)

                let isSkipping =
                    ForEach.makeIsSkipping seqComponent

                let terminatingIdx =
                    getTerminatingIdx count

                let mutable maybeSkipping = true
                let mutable idx = -1

                let rec moveNext () =
                    if (result.HaltedIdx = 0) && idx < terminatingIdx then
                        idx <- idx + 1

                        if maybeSkipping then
                            // Skip can only is only checked at the start of the sequence, so once
                            // triggered, we stay triggered.
                            maybeSkipping <- isSkipping ()

                        if maybeSkipping then
                            moveNext ()
                        elif seqComponent.ProcessNext (f idx) then
                            true
                        else
                            moveNext ()
                    elif (result.HaltedIdx = 0) && idx = System.Int32.MaxValue then
                        raise <| System.InvalidOperationException (SR.GetString(SR.enumerationPastIntMaxValue))
                    else
                        result.SeqState <- SeqProcessNextStates.Finished
                        let mutable stopTailCall = ()
                        (seqComponent).ChainComplete (&stopTailCall, result.HaltedIdx)
                        false

                interface IEnumerator with
                    member __.MoveNext () =
                        result.SeqState <- SeqProcessNextStates.InProcess
                        moveNext ()

            type Enumerable<'T,'U>(count:Nullable<int>, f:int->'T, current:SeqFactory<'T,'U>) =
                inherit Enumerable.EnumerableBase<'U>()

                interface IEnumerable<'U> with
                    member this.GetEnumerator () : IEnumerator<'U> =
                        let result = Result<'U> ()
                        Upcast.enumerator (new Enumerator<'T,'U>(count, f, current.Build (Upcast.outOfBand result) result, result))

                interface ISeq<'U> with
                    member this.Compose (next:SeqFactory<'U,'V>) : ISeq<'V> =
                        Upcast.seq (new Enumerable<'T,'V>(count, f, ComposedFactory.Combine current next))

                    member this.ForEach<'Result,'State> (createResult:PipeIdx->Folder<'U,'Result,'State>) =
                        let terminatingIdx = getTerminatingIdx count
                        ForEach.execute createResult current (ForEach.init (f, terminatingIdx))

            let upto lastOption f =
                match lastOption with
                | Some b when b<0 -> failwith "library implementation error: upto can never be called with a negative value"
                | _ ->
                    let unstarted   = -1  // index value means unstarted (and no valid index)
                    let completed   = -2  // index value means completed (and no valid index)
                    let unreachable = -3  // index is unreachable from 0,1,2,3,...
                    let finalIndex  = match lastOption with
                                        | Some b -> b             // here b>=0, a valid end value.
                                        | None   -> unreachable   // run "forever", well as far as Int32.MaxValue since indexing with a bounded type.
                    // The Current value for a valid index is "f i".
                    // Lazy<_> values are used as caches, to store either the result or an exception if thrown.
                    // These "Lazy<_>" caches are created only on the first call to current and forced immediately.
                    // The lazy creation of the cache nodes means enumerations that skip many Current values are not delayed by GC.
                    // For example, the full enumeration of Seq.initInfinite in the tests.
                    // state
                    let index   = ref unstarted
                    // a Lazy node to cache the result/exception
                    let current = ref (Unchecked.defaultof<_>)
                    let setIndex i = index := i; current := (Unchecked.defaultof<_>) // cache node unprimed, initialised on demand.
                    let getCurrent() =
                        if !index = unstarted then notStarted()
                        if !index = completed then alreadyFinished()
                        match box !current with
                        | null -> current := Lazy<_>.Create(fun () -> f !index)
                        | _ ->  ()
                        // forced or re-forced immediately.
                        (!current).Force()
                    { new IEnumerator<'U> with
                            member x.Current = getCurrent()
                        interface IEnumerator with
                            member x.Current = box (getCurrent())
                            member x.MoveNext() =
                                if !index = completed then
                                    false
                                elif !index = unstarted then
                                    setIndex 0
                                    true
                                else (
                                    if !index = System.Int32.MaxValue then raise <| System.InvalidOperationException (SR.GetString(SR.enumerationPastIntMaxValue))
                                    if !index = finalIndex then
                                        false
                                    else
                                        setIndex (!index + 1)
                                        true
                                )
                            member self.Reset() = noReset()
                        interface System.IDisposable with
                            member x.Dispose() = () }

            type EnumerableDecider<'T>(count:Nullable<int>, f:int->'T) =
                inherit Enumerable.EnumerableBase<'T>()

                interface IEnumerable<'T> with
                    member this.GetEnumerator () : IEnumerator<'T> =
                        // we defer back to the original implementation as, as it's quite idiomatic in it's decision
                        // to calculate Current in a lazy fashion. I doubt anyone is really using this functionality
                        // in the way presented, but it's possible.
                        upto (if count.HasValue then Some (count.Value-1) else None) f

                interface ISeq<'T> with
                    member this.Compose (next:SeqFactory<'T,'U>) : ISeq<'U> =
                        Upcast.seq (Enumerable<'T,'V>(count, f, next))

                    member this.ForEach<'Result,'State> (f:PipeIdx->Folder<'T,'Result,'State>) =
                        ForEach.executeThin f (ForEach.enumerable (Upcast.enumerable this))

        [<CompiledName "ToComposer">]
        let toComposer (source:seq<'T>) : ISeq<'T> =
            match source with
            | :? ISeq<'T> as s -> s
            | :? array<'T> as a -> Upcast.seq (Array.Enumerable((fun () -> a), IdentityFactory.Instance))
            | :? list<'T> as a -> Upcast.seq (List.Enumerable(a, IdentityFactory.Instance))
            | null -> nullArg "source"
            | _ -> Upcast.seq (Enumerable.EnumerableThin<'T> source)

        let inline foreach f (source:ISeq<_>) = source.ForEach f
        let inline compose (factory:#SeqFactory<_,_>) (source:ISeq<'T>) = source.Compose factory

        [<CompiledName "Average">]
        let inline average (source: ISeq< ^T>) : ^T
            when ^T:(static member Zero : ^T)
            and  ^T:(static member (+) : ^T * ^T -> ^T)
            and  ^T:(static member DivideByInt : ^T * int -> ^T) =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup< ^T, ^T, int> (LanguagePrimitives.GenericZero, 0) with
                    override this.ProcessNext value =
                        this.Result <- Checked.(+) this.Result value
                        this.State <- this.State + 1
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State = 0 then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                        this.Result <- LanguagePrimitives.DivideByInt< ^T> this.Result this.State 
                    override this.OnDispose () = () })

        [<CompiledName "AverageBy">]
        let inline averageBy (f : 'T -> ^U) (source: ISeq< 'T >) : ^U
            when ^U:(static member Zero : ^U)
            and  ^U:(static member (+) : ^U * ^U -> ^U)
            and  ^U:(static member DivideByInt : ^U * int -> ^U) =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup<'T,'U, int>(LanguagePrimitives.GenericZero, 0) with
                    override this.ProcessNext value =
                        this.Result <- Checked.(+) this.Result (f value)
                        this.State <- this.State + 1
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State = 0 then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                        this.Result <- LanguagePrimitives.DivideByInt< ^U> this.Result this.State
                    override this.OnDispose () = () })

        [<CompiledName "Empty">]
        let empty<'T> = EmptyEnumerable.Enumerable<'T>.Instance

        [<CompiledName "ExactlyOne">]
        let inline exactlyOne errorString  (source : ISeq<'T>) : 'T =
            source
            |> foreach (fun pipeIdx ->
                upcast { new FolderWithCleanup<'T,'T,Values<bool, bool>>(Unchecked.defaultof<'T>, Values<bool,bool>(true, false)) with
                    override this.ProcessNext value =
                        if this.State._1 then
                            this.State._1 <- false
                            this.Result <- value
                        else
                            this.State._2 <- true
                            this.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State._1 then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                        elif this.State._2 then
                            invalidArg "source" errorString
                    override this.OnDispose () = () })

        [<CompiledName "Fold">]
        let inline fold<'T,'State> (f:'State->'T->'State) (seed:'State) (source:ISeq<'T>) : 'State =
            source
            |> foreach (fun _ ->
                upcast { new Folder<'T,'State>(seed) with
                    override this.ProcessNext value =
                        this.Result <- f this.Result value
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "Fold2">]
        let inline fold2<'T1,'T2,'State> (folder:'State->'T1->'T2->'State) (state:'State) (source1: ISeq<'T1>) (source2: ISeq<'T2>) =
            source1
            |> foreach (fun pipeIdx ->
                upcast { new FolderWithCleanup<_,'State,IEnumerator<'T2>>(state,source2.GetEnumerator()) with
                    override self.ProcessNext value =
                        if self.State.MoveNext() then
                            self.Result <- folder self.Result value self.State.Current
                        else
                            self.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override self.OnComplete _ = ()
                    override self.OnDispose () = self.State.Dispose() })

        [<CompiledName "Unfold">]
        let unfold (generator:'State->option<'T * 'State>) (state:'State) : ISeq<'T> =
            Upcast.seq (new Unfold.Enumerable<'T,'T,'State>(generator, state, IdentityFactory.Instance))

        [<CompiledName "InitializeInfinite">]
        let initInfinite<'T> (f:int->'T) : ISeq<'T> =
            Upcast.seq (new Init.EnumerableDecider<'T>(Nullable (), f))

        [<CompiledName "Initialize">]
        let init<'T> (count:int) (f:int->'T) : ISeq<'T> =
            if count < 0 then invalidArgInputMustBeNonNegative "count" count
            elif count = 0 then empty else
            Upcast.seq (new Init.EnumerableDecider<'T>(Nullable count, f))

        [<CompiledName "Iterate">]
        let iter f (source:ISeq<'T>) =
            source
            |> foreach (fun _ ->
                upcast { new Folder<'T,NoValue> (Unchecked.defaultof<NoValue>) with
                    override this.ProcessNext value =
                        f value
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })
            |> ignore

        [<CompiledName "Iterate2">]
        let inline iter2 (f:'T->'U->unit) (source1:ISeq<'T>) (source2:ISeq<'U>) : unit =
            source1
            |> foreach (fun pipeIdx ->
                upcast { new FolderWithCleanup<'T,NoValue,IEnumerator<'U>> (Unchecked.defaultof<_>,source2.GetEnumerator()) with
                    override self.ProcessNext value =
                        if self.State.MoveNext() then
                            f value self.State.Current
                        else
                            self.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override self.OnComplete _ = ()
                    override self.OnDispose () = self.State.Dispose() })
            |> ignore

        [<CompiledName "IterateIndexed2">]
        let inline iteri2 (f:int->'T->'U->unit) (source1:ISeq<'T>) (source2:ISeq<'U>) : unit =
            source1
            |> foreach (fun pipeIdx ->
                upcast { new FolderWithCleanup<'T,NoValue,Values<int,IEnumerator<'U>>>(Unchecked.defaultof<_>,Values<_,_>(-1,source2.GetEnumerator())) with
                    override self.ProcessNext value =
                        if self.State._2.MoveNext() then
                            f self.State._1 value self.State._2.Current
                            self.State._1 <- self.State._1 + 1
                            Unchecked.defaultof<_>
                        else
                            self.StopFurtherProcessing pipeIdx
                            Unchecked.defaultof<_>
                    override self.OnComplete _ = () 
                    override self.OnDispose () = self.State._2.Dispose() })
            |> ignore

        [<CompiledName "TryHead">]
        let tryHead (source:ISeq<'T>) =
            source
            |> foreach (fun pipeIdx ->
                upcast { new Folder<'T, Option<'T>> (None) with
                    override this.ProcessNext value =
                        this.Result <- Some value
                        this.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "IterateIndexed">]
        let iteri f (source:ISeq<'T>) =
            source
            |> foreach (fun _ ->
                { new Folder<'T,NoValue,int> (Unchecked.defaultof<_>,0) with
                    override this.ProcessNext value =
                        f this.State value
                        this.State <- this.State + 1
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })
            |> ignore

        [<CompiledName "Except">]
        let inline except (itemsToExclude: seq<'T>) (source:ISeq<'T>) : ISeq<'T> when 'T:equality =
            source |> compose { new SeqFactory<'T,'T>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChainedWithState<'T,'V,Lazy<HashSet<'T>>>
                                    (next,lazy(HashSet<'T>(itemsToExclude,HashIdentity.Structural<'T>))) with
                        override this.ProcessNext (input:'T) : bool =
                            if this.State.Value.Add input then TailCall.avoid (next.ProcessNext input)
                            else false }}

        [<CompiledName "Exists">]
        let exists f (source:ISeq<'T>) =
            source
            |> foreach (fun pipeIdx ->
                upcast { new Folder<'T, bool> (false) with
                    override this.ProcessNext value =
                        if f value then
                            this.Result <- true
                            this.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "Exists2">]
        let exists2 (predicate:'T->'U->bool) (source1: ISeq<'T>) (source2: ISeq<'U>) : bool =
            source1
            |> foreach (fun pipeIdx ->
                upcast { new FolderWithCleanup<'T,bool,IEnumerator<'U>>(false,source2.GetEnumerator()) with
                    override self.ProcessNext value =
                        if self.State.MoveNext() then
                            if predicate value self.State.Current then
                                self.Result <- true
                                self.StopFurtherProcessing pipeIdx
                        else
                            self.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override self.OnComplete _ = ()
                    override self.OnDispose () = self.State.Dispose() })

        [<CompiledName "Contains">]
        let inline contains element (source:ISeq<'T>) =
            source
            |> foreach (fun pipeIdx ->
                upcast { new Folder<'T, bool> (false) with
                    override this.ProcessNext value =
                        if element = value then
                            this.Result <- true
                            this.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "ForAll">]
        let forall predicate (source:ISeq<'T>) =
            source
            |> foreach (fun pipeIdx ->
                upcast { new Folder<'T, bool> (true) with
                    override this.ProcessNext value =
                        if not (predicate value) then
                            this.Result <- false
                            this.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "ForAll2">]
        let inline forall2 predicate (source1:ISeq<'T>) (source2:ISeq<'U>) : bool =
            source1
            |> foreach (fun pipeIdx ->
                upcast { new FolderWithCleanup<'T,bool,IEnumerator<'U>>(true,source2.GetEnumerator()) with
                    override self.ProcessNext value =
                        if self.State.MoveNext() then
                            if not (predicate value self.State.Current) then
                                self.Result <- false
                                self.StopFurtherProcessing pipeIdx
                        else
                            self.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override self.OnComplete _ = ()
                    override self.OnDispose () = self.State.Dispose() })

        [<CompiledName "Filter">]
        let inline filter<'T> (f:'T->bool) (source:ISeq<'T>) : ISeq<'T> =
            source |> compose { new SeqFactory<'T,'T>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChained<'T,'V>(next) with
                        override __.ProcessNext input =
                            if f input then TailCall.avoid (next.ProcessNext input)
                            else false } }

        [<CompiledName "Map">]
        let inline map<'T,'U> (f:'T->'U) (source:ISeq<'T>) : ISeq<'U> =
            source |> compose { new SeqFactory<'T,'U>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChained<'T,'V>(next) with
                        override __.ProcessNext input =
                            TailCall.avoid (next.ProcessNext (f input)) } }

        [<CompiledName "MapIndexed">]
        let inline mapi f source =
            source |> compose { new SeqFactory<'T,'U>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChainedWithState<'T,'V,int>(next, -1) with
                        override this.ProcessNext (input:'T) : bool =
                            this.State <- this.State  + 1
                            TailCall.avoid (next.ProcessNext (f this.State input)) } }

        [<CompiledName "Map2">]
        let inline map2<'First,'Second,'U> (map:'First->'Second->'U) (source1:ISeq<'First>) (source2:ISeq<'Second>) : ISeq<'U> =
            source1 |> compose { new SeqFactory<'First,'U>() with
                override __.Create<'V> outOfBand pipeIdx next =
                    upcast { new ConsumerChainedWithStateAndCleanup<'First,'V, IEnumerator<'Second>>(next, (source2.GetEnumerator ())) with
                        override self.ProcessNext input =
                            if self.State.MoveNext () then
                                TailCall.avoid (next.ProcessNext (map input self.State.Current))
                            else
                                outOfBand.StopFurtherProcessing pipeIdx
                                false
                        override self.OnComplete _ = () 
                        override self.OnDispose () = self.State.Dispose () }}

        [<CompiledName "MapIndexed2">]
        let inline mapi2<'First,'Second,'U> (map:int -> 'First->'Second->'U) (source1:ISeq<'First>) (source2:ISeq<'Second>) : ISeq<'U> =
            source1 |> compose { new SeqFactory<'First,'U>() with
                override __.Create<'V> outOfBand pipeIdx next =
                    upcast { new ConsumerChainedWithStateAndCleanup<'First,'V, Values<int,IEnumerator<'Second>>>
                                                (next, Values<_,_>(-1, source2.GetEnumerator ())) with
                        override self.ProcessNext input =
                            if self.State._2.MoveNext () then
                                self.State._1 <- self.State._1 + 1
                                TailCall.avoid (next.ProcessNext (map self.State._1 input self.State._2.Current))
                            else
                                outOfBand.StopFurtherProcessing pipeIdx
                                false
                        override self.OnDispose () = self.State._2.Dispose ()
                        override self.OnComplete _ = () }}

        [<CompiledName "Map3">]
        let inline map3<'First,'Second,'Third,'U>
                        (map:'First->'Second->'Third->'U) (source1:ISeq<'First>) (source2:ISeq<'Second>) (source3:ISeq<'Third>) : ISeq<'U> =
            source1 |> compose { new SeqFactory<'First,'U>() with
                override __.Create<'V> outOfBand pipeIdx next =
                    upcast { new ConsumerChainedWithStateAndCleanup<'First,'V, Values<IEnumerator<'Second>,IEnumerator<'Third>>>
                                                (next, Values<_,_>(source2.GetEnumerator(),source3.GetEnumerator())) with
                        override self.ProcessNext input =
                            if self.State._1.MoveNext() && self.State._2.MoveNext ()  then
                                TailCall.avoid (next.ProcessNext (map input self.State._1 .Current self.State._2.Current))
                            else
                                outOfBand.StopFurtherProcessing pipeIdx
                                false
                        override self.OnComplete _ = () 
                        override self.OnDispose () = 
                            self.State._1.Dispose ()
                            self.State._2.Dispose () }}


        [<CompiledName "CompareWith">]
        let inline compareWith (f:'T -> 'T -> int) (source1 :ISeq<'T>) (source2:ISeq<'T>) : int =
            source1
            |> foreach (fun pipeIdx ->
                upcast { new FolderWithCleanup<'T,int,IEnumerator<'T>>(0,source2.GetEnumerator()) with
                    override self.ProcessNext value =
                        if not (self.State.MoveNext()) then
                            self.Result <- 1
                            self.StopFurtherProcessing pipeIdx
                        else
                            let c = f value self.State.Current
                            if c <> 0 then
                                self.Result <- c
                                self.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)
                    override self.OnComplete _ =
                        if self.Result = 0 && self.State.MoveNext() then
                            self.Result <- -1
                    override self.OnDispose () = self.State.Dispose() })

        [<CompiledName "Choose">]
        let inline choose (f:'T->option<'U>) (source:ISeq<'T>) : ISeq<'U> =
            source |> compose { new SeqFactory<'T,'U>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChained<'T,'V>(next) with
                        override __.ProcessNext input =
                            match f input with
                            | Some value -> TailCall.avoid (next.ProcessNext value)
                            | None       -> false } }

        [<CompiledName "Distinct">]
        let inline distinct (source:ISeq<'T>) : ISeq<'T> when 'T:equality =
            source |> compose { new SeqFactory<'T,'T>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChainedWithState<'T,'V,HashSet<'T>>
                                    (next,(HashSet<'T>(HashIdentity.Structural<'T>))) with
                        override this.ProcessNext (input:'T) : bool =
                            if this.State.Add input then TailCall.avoid (next.ProcessNext input)
                            else false } }

        [<CompiledName "DistinctBy">]
        let inline distinctBy (keyf:'T->'Key) (source:ISeq<'T>) :ISeq<'T>  when 'Key:equality =
            source |> compose { new SeqFactory<'T,'T>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChainedWithState<'T,'V,HashSet<'Key>>
                                    (next,(HashSet<'Key>(HashIdentity.Structural<'Key>))) with
                        override this.ProcessNext (input:'T) : bool =
                            if this.State.Add (keyf input) then TailCall.avoid (next.ProcessNext input)
                            else false } }

        [<CompiledName "Max">]
        let inline max (source: ISeq<'T>) : 'T when 'T:comparison =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup<'T,'T,bool>(Unchecked.defaultof<'T>,true) with
                    override this.ProcessNext value =
                        if this.State then
                            this.State <- false
                            this.Result <- value
                        elif value > this.Result then
                            this.Result <- value
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                    override self.OnDispose () = () })

        [<CompiledName "MaxBy">]
        let inline maxBy (f :'T -> 'U) (source: ISeq<'T>) : 'T when 'U:comparison =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup<'T,'T,Values<bool,'U>>(Unchecked.defaultof<'T>,Values<_,_>(true,Unchecked.defaultof<'U>)) with
                    override this.ProcessNext value =
                        match this.State._1, f value with
                        | true, valueU ->
                            this.State._1 <- false
                            this.State._2 <- valueU
                            this.Result <- value
                        | false, valueU when valueU > this.State._2 ->
                            this.State._2 <- valueU
                            this.Result <- value
                        | _ -> ()
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State._1 then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                    override self.OnDispose () = () })

        [<CompiledName "Min">]
        let inline min (source: ISeq< 'T>) : 'T when 'T:comparison =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup<'T,'T,bool>(Unchecked.defaultof<'T>,true) with
                    override this.ProcessNext value =
                        if this.State then
                            this.State <- false
                            this.Result <- value
                        elif value < this.Result then
                            this.Result <- value
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                    override self.OnDispose () = () })

        [<CompiledName "MinBy">]
        let inline minBy (f : 'T -> 'U) (source: ISeq<'T>) : 'T =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup<'T,'T,Values<bool,'U>>(Unchecked.defaultof<'T>,Values<_,_>(true,Unchecked.defaultof< 'U>)) with
                    override this.ProcessNext value =
                        match this.State._1, f value with
                        | true, valueU ->
                            this.State._1 <- false
                            this.State._2 <- valueU
                            this.Result <- value
                        | false, valueU when valueU < this.State._2 ->
                            this.State._2 <- valueU
                            this.Result <- value
                        | _ -> ()
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State._1 then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                    override self.OnDispose () = () })

        [<CompiledName "Pairwise">]
        let inline pairwise (source:ISeq<'T>) : ISeq<'T * 'T> =
            source |> compose { new SeqFactory<'T,'T * 'T>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChainedWithState<'T,'U,Values<bool,'T>>
                                (   next
                                ,   Values<bool,'T>
                                    ((* isFirst   = _1*) true
                                    ,(* lastValue = _2*) Unchecked.defaultof<'T>
                                    )
                                ) with
                            override self.ProcessNext (input:'T) : bool =
                                if (*isFirst*) self.State._1  then
                                    self.State._2 (*lastValue*)<- input
                                    self.State._1 (*isFirst*)<- false
                                    false
                                else
                                    let currentPair = self.State._2, input
                                    self.State._2 (*lastValue*)<- input
                                    TailCall.avoid (next.ProcessNext currentPair) }}

        [<CompiledName "Reduce">]
        let inline reduce (f:'T->'T->'T) (source : ISeq<'T>) : 'T =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup<'T,'T,bool>(Unchecked.defaultof<'T>,true) with
                    override this.ProcessNext value =
                        if this.State then
                            this.State <- false
                            this.Result <- value
                        else
                            this.Result <- f this.Result value
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)

                    override this.OnComplete _ =
                        if this.State then
                            invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
                    override self.OnDispose () = () })

        [<CompiledName "Scan">]
        let inline scan (folder:'State->'T->'State) (initialState:'State) (source:ISeq<'T>) :ISeq<'State> =
            source |> compose { new SeqFactory<'T,'State>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChainedWithState<'T,'V,'State>(next, initialState) with
                        override this.ProcessNext (input:'T) : bool =
                            this.State <- folder this.State input
                            TailCall.avoid (next.ProcessNext this.State) } }


        [<CompiledName "Skip">]
        let inline skip (errorString:string) (skipCount:int) (source:ISeq<'T>) : ISeq<'T> =
            source |> compose { new SeqFactory<'T,'T>() with
                override __.Create _ _ next =
                    upcast {
                        new ConsumerChainedWithStateAndCleanup<'T,'U,int>(next,(*count*)0) with

                            override self.ProcessNext (input:'T) : bool =
                                if (*count*) self.State < skipCount then
                                    self.State <- self.State + 1
                                    false
                                else
                                    TailCall.avoid (next.ProcessNext input)

                            override self.OnComplete _ =
                                if (*count*) self.State < skipCount then
                                    let x = skipCount - self.State
                                    invalidOpFmt "{0}\ntried to skip {1} {2} past the end of the seq"
                                        [|errorString; x; (if x=1 then "element" else "elements")|]
                            override self.OnDispose () = ()

                        interface ISkipping with
                            member self.Skipping () =
                                let self = self :?> ConsumerChainedWithState<'T,'U,int>
                                if (*count*) self.State < skipCount then
                                    self.State <- self.State + 1
                                    true
                                else
                                    false
                    }}

        [<CompiledName "SkipWhile">]
        let inline skipWhile (predicate:'T->bool) (source:ISeq<'T>) : ISeq<'T> =
            source |> compose { new SeqFactory<'T,'T>() with
                override __.Create _ _ next =
                    upcast { new ConsumerChainedWithState<'T,'V,bool>(next,true) with
                        override self.ProcessNext (input:'T) : bool =
                            if self.State (*skip*) then
                                self.State <- predicate input
                                if self.State (*skip*) then
                                    false
                                else
                                    TailCall.avoid (next.ProcessNext input)
                            else
                                TailCall.avoid (next.ProcessNext input) }}

        [<CompiledName "Sum">]
        let inline sum (source:ISeq< ^T>) : ^T
            when ^T:(static member Zero : ^T)
            and  ^T:(static member (+) :  ^T *  ^T ->  ^T) =
            source
            |> foreach (fun _ ->
                upcast { new Folder< ^T,^T> (LanguagePrimitives.GenericZero) with
                    override this.ProcessNext value =
                        this.Result <- Checked.(+) this.Result value
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "SumBy">]
        let inline sumBy (f : 'T -> ^U) (source: ISeq<'T>) : ^U
            when ^U:(static member Zero : ^U)
            and  ^U:(static member (+) :  ^U *  ^U ->  ^U) =
            source
            |> foreach (fun _ ->
                upcast { new Folder<'T,'U> (LanguagePrimitives.GenericZero< ^U>) with
                    override this.ProcessNext value =
                        this.Result <- Checked.(+) this.Result (f value)
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "Take">]
        let inline take (errorString:string) (takeCount:int) (source:ISeq<'T>) : ISeq<'T> =
            source |> compose { new SeqFactory<'T,'T>() with
                member __.Create outOfBand pipelineIdx next =
                    upcast {
                        new ConsumerChainedWithStateAndCleanup<'T,'U,int>(next,(*count*)0) with
                            override self.ProcessNext (input:'T) : bool =
                                if (*count*) self.State < takeCount then
                                    self.State <- self.State + 1
                                    if self.State = takeCount then
                                        outOfBand.StopFurtherProcessing pipelineIdx
                                    TailCall.avoid (next.ProcessNext input)
                                else
                                    outOfBand.StopFurtherProcessing pipelineIdx
                                    false

                            override this.OnComplete terminatingIdx =
                                if terminatingIdx < pipelineIdx && this.State < takeCount then
                                    let x = takeCount - this.State
                                    invalidOpFmt "tried to take {0} {1} past the end of the seq"
                                        [|errorString; x; (if x=1 then "element" else "elements")|]
                            override this.OnDispose () = () }}

        [<CompiledName "TakeWhile">]
        let inline takeWhile (predicate:'T->bool) (source:ISeq<'T>) : ISeq<'T> =
            source |> compose { new SeqFactory<'T,'T>() with
                member __.Create outOfBand pipeIdx next =
                    upcast { new ConsumerChained<'T,'V>(next) with
                        override __.ProcessNext (input:'T) : bool =
                            if predicate input then
                                TailCall.avoid (next.ProcessNext input)
                            else
                                outOfBand.StopFurtherProcessing pipeIdx
                                false
                    }}

        [<CompiledName "Tail">]
        let inline tail errorString (source:ISeq<'T>) :ISeq<'T> =
            source |> compose { new SeqFactory<'T,'T>() with
                member __.Create _ _ next =
                    upcast { new ConsumerChainedWithStateAndCleanup<'T,'V,bool>(next,(*first*) true) with
                        override self.ProcessNext (input:'T) : bool =
                            if (*first*) self.State then
                                self.State <- false
                                false
                            else
                                TailCall.avoid (next.ProcessNext input)

                        override self.OnComplete _ =
                            if (*first*) self.State then
                                invalidArg "source" errorString 
                        override self.OnDispose () = () }}

        [<CompiledName "Truncate">]
        let inline truncate (truncateCount:int) (source:ISeq<'T>) : ISeq<'T> =
            source |> compose { new SeqFactory<'T,'T>() with
                member __.Create outOfBand pipeIdx next =
                    upcast {
                        new ConsumerChainedWithState<'T,'U,int>(next,(*count*)0) with
                            override self.ProcessNext (input:'T) : bool =
                                if (*count*) self.State < truncateCount then
                                    self.State <- self.State + 1
                                    if self.State = truncateCount then
                                        outOfBand.StopFurtherProcessing pipeIdx
                                    TailCall.avoid (next.ProcessNext input)
                                else
                                    outOfBand.StopFurtherProcessing pipeIdx
                                    false }}

        [<CompiledName "Indexed">]
        let indexed source =
            mapi (fun i x -> i,x) source

        [<CompiledName "TryItem">]
        let tryItem (errorString:string) index (source:ISeq<'T>) =
            if index < 0 then None else
            source |> skip errorString index |> tryHead

        [<CompiledName "TryPick">]
        let tryPick f (source:ISeq<'T>)  =
            source
            |> foreach (fun pipeIdx ->
                upcast { new Folder<'T, Option<'U>> (None) with
                    override this.ProcessNext value =
                        match f value with
                        | (Some _) as some ->
                            this.Result <- some
                            this.StopFurtherProcessing pipeIdx
                        | None -> ()
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "TryFind">]
        let tryFind f (source:ISeq<'T>)  =
            source
            |> foreach (fun pipeIdx ->
                upcast { new Folder<'T, Option<'T>> (None) with
                    override this.ProcessNext value =
                        if f value then
                            this.Result <- Some value
                            this.StopFurtherProcessing pipeIdx
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "TryFindIndex">]
        let inline tryFindIndex (predicate:'T->bool) (source:ISeq<'T>) : int option =
            source
            |> foreach (fun pipeIdx ->
                { new Folder<'T, Option<int>, int>(None, 0) with
                    override this.ProcessNext value =
                        if predicate value then
                            this.Result <- Some this.State
                            this.StopFurtherProcessing pipeIdx
                        else
                            this.State <- this.State + 1
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *) })

        [<CompiledName "TryLast">]
        let inline tryLast (source :ISeq<'T>) : 'T option =
            source
            |> foreach (fun _ ->
                upcast { new FolderWithCleanup<'T,option<'T>,Values<bool,'T>>(None,Values<bool,'T>(true, Unchecked.defaultof<'T>)) with
                    override this.ProcessNext value =
                        if this.State._1 then
                            this.State._1 <- false
                        this.State._2 <- value
                        Unchecked.defaultof<_> (* return value unsed in ForEach context *)
                    override this.OnComplete _ =
                        if not this.State._1 then
                            this.Result <- Some this.State._2
                    override self.OnDispose () = () })

        [<CompiledName "Windowed">]
        let inline windowed (windowSize:int) (source:ISeq<'T>) : ISeq<'T[]> =
            source |> compose { new SeqFactory<'T,'T[]>() with
                member __.Create outOfBand pipeIdx next =
                    upcast {
                        new ConsumerChainedWithState<'T,'U,Values<'T[],int,int>>
                                    (   next
                                    ,   Values<'T[],int,int>
                                        ((*circularBuffer = _1 *) Array.zeroCreateUnchecked windowSize
                                        ,(* idx = _2 *)          0
                                        ,(* priming = _3 *)      windowSize-1
                                        )
                                    ) with
                            override self.ProcessNext (input:'T) : bool =
                                self.State._1.[(* idx *)self.State._2] <- input

                                self.State._2 <- (* idx *)self.State._2 + 1
                                if (* idx *) self.State._2 = windowSize then
                                    self.State._2 <- 0

                                if (* priming  *) self.State._3 > 0 then
                                    self.State._3 <- self.State._3 - 1
                                    false
                                else
                                    if windowSize < 32 then
                                        let window :'T [] = Array.init windowSize (fun i -> self.State._1.[((* idx *)self.State._2+i) % windowSize]: 'T)
                                        TailCall.avoid (next.ProcessNext window)
                                    else
                                        let window = Array.zeroCreateUnchecked windowSize
                                        Array.Copy((*circularBuffer*)self.State._1, (* idx *)self.State._2, window, 0, windowSize - (* idx *)self.State._2)
                                        Array.Copy((*circularBuffer*)self.State._1, 0, window, windowSize - (* idx *)self.State._2, (* idx *)self.State._2)
                                        TailCall.avoid (next.ProcessNext window)

                    }}

// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace SeqComposer.Microsoft.FSharp.Collections

    open System
    open System.Diagnostics
    open System.Collections
    open System.Collections.Generic
    open System.Reflection
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics

    open SeqComposer.Microsoft.FSharp.Patches
    open SeqComposer.Microsoft.FSharp.Collections.Composer
    open SeqComposer.Microsoft.FSharp.Collections.Composer.Core
    open SeqComposer.Microsoft.FSharp.Collections.IEnumerator
    open SeqComposer.Microsoft.FSharp.Core.CompilerServices

    module Upcast =
        // The f# compiler outputs unnecessary unbox.any calls in upcasts. If this functionality
        // is fixed with the compiler then these functions can be removed.
        let inline enumerable (t:#IEnumerable<'T>) : IEnumerable<'T> = (# "" t : IEnumerable<'T> #)

    [<Sealed>]
    type CachedSeq<'T>(cleanup,res:seq<'T>) =
        interface System.IDisposable with
            member x.Dispose() = cleanup()
        interface System.Collections.Generic.IEnumerable<'T> with
            member x.GetEnumerator() = res.GetEnumerator()
        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = (res :> System.Collections.IEnumerable).GetEnumerator()
        member obj.Clear() = cleanup()


    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Seq =
#if FX_NO_ICLONEABLE
        open Microsoft.FSharp.Core.ICloneableExtensions
#else
#endif
        let mkDelayedSeq (f: unit -> IEnumerable<'T>) = mkSeq (fun () -> f().GetEnumerator())
        let inline indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))

        [<CompiledName("ToComposer")>]
        let toComposer (source:seq<'T>): Composer.Core.ISeq<'T> =
            Composer.toComposer source

        let inline foreach f (source:seq<_>) =
            Composer.foreach f (toComposer source)

        let private seqFactory (createSeqComponent:#SeqFactory<_,_>) (source:seq<'T>) =
            match source with
            | :? Composer.Core.ISeq<'T> as s -> Upcast.enumerable (s.Compose createSeqComponent)
            | :? array<'T> as a -> Upcast.enumerable (Composer.Array.create a createSeqComponent)
            | :? list<'T> as a -> Upcast.enumerable (Composer.List.create a createSeqComponent)
            | null -> nullArg "source"
            | _ -> Upcast.enumerable (Composer.Enumerable.create source createSeqComponent)

        [<CompiledName("Delay")>]
        let delay f = mkDelayedSeq f

        [<CompiledName("Unfold")>]
        let unfold (generator:'State->option<'T * 'State>) (state:'State) : seq<'T> =
            Composer.unfold generator state
            |> Upcast.enumerable

        [<CompiledName("Empty")>]
        let empty<'T> = (EmptyEnumerable :> seq<'T>)

        [<CompiledName("InitializeInfinite")>]
        let initInfinite<'T> (f:int->'T) : IEnumerable<'T> =
            Composer.initInfinite f
            |> Upcast.enumerable

        [<CompiledName("Initialize")>]
        let init<'T> (count:int) (f:int->'T) : IEnumerable<'T> =
            Composer.init count f
            |> Upcast.enumerable

        [<CompiledName("Iterate")>]
        let iter f (source : seq<'T>) =
            source |> toComposer |> Composer.iter f

        [<CompiledName("TryHead")>]
        let tryHead (source : seq<_>) =
            source |> toComposer |> Composer.tryHead

        [<CompiledName("Skip")>]
        let skip count (source: seq<_>) =
            source |> toComposer
            |> Composer.skip (SR.GetString SR.notEnoughElements) count |> Upcast.enumerable

//        let invalidArgumnetIndex = invalidArgFmt "index" PATCHHED

        [<CompiledName("Item")>]
        let item i (source : seq<'T>) =
            if i < 0 then invalidArgInputMustBeNonNegative "index" i else
                source
                |> toComposer |> Composer.skip (SR.GetString SR.notEnoughElements) i |> Upcast.enumerable
                |> tryHead
                |>  function
                    | None -> invalidArgFmt "index" "{0}\nseq was short by 1 element"  [|SR.GetString SR.notEnoughElements|]
                    | Some value -> value

        [<CompiledName("TryItem")>]
        let tryItem i (source:seq<'T>) =
            source |> toComposer |> Composer.tryItem (SR.GetString SR.notEnoughElements) i

        [<CompiledName "Get">]
        let nth i (source : seq<'T>) = item i source

        [<CompiledName "IterateIndexed">]
        let iteri f (source:seq<'T>) =
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
            source |> toComposer |> Composer.iteri (fun idx a -> f.Invoke(idx,a))

        [<CompiledName "Exists">]
        let exists f (source:seq<'T>) =
            source |> toComposer |> Composer.exists f

        [<CompiledName "Contains">]
        let inline contains element (source:seq<'T>) =
            source |> toComposer |> Composer.contains element

        [<CompiledName "ForAll">]
        let forall f (source:seq<'T>) =
            source |> toComposer |> Composer.forall f

        [<CompiledName "Iterate2">]
        let iter2 (f:'T->'U->unit) (source1 : seq<'T>) (source2 : seq<'U>)    =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            (source1|>toComposer, source2|>toComposer)
            ||> Composer.iter2 (fun a b -> f.Invoke(a,b))


        [<CompiledName "IterateIndexed2">]
        let iteri2 (f:int->'T->'U->unit)  (source1 : seq<_>) (source2 : seq<_>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            (source1|>toComposer, source2|>toComposer)
            ||> Composer.iteri2 (fun idx a b -> f.Invoke(idx,a,b))


        // Build an IEnumerble by wrapping/transforming iterators as they get generated.
        let revamp f (ie : seq<_>) = mkSeq (fun () -> f (ie.GetEnumerator()))
        let revamp2 f (ie1 : seq<_>) (source2 : seq<_>) =
            mkSeq (fun () -> f (ie1.GetEnumerator()) (source2.GetEnumerator()))
        let revamp3 f (ie1 : seq<_>) (source2 : seq<_>) (source3 : seq<_>) =
            mkSeq (fun () -> f (ie1.GetEnumerator()) (source2.GetEnumerator()) (source3.GetEnumerator()))

        [<CompiledName("Filter")>]
        let filter<'T> (f:'T->bool) (source:seq<'T>) : seq<'T> =
            source |> toComposer |> Composer.filter f |> Upcast.enumerable

        [<CompiledName("Where")>]
        let where f source = filter f source

        [<CompiledName "Map">]
        let map<'T,'U> (f:'T->'U) (source:seq<'T>) : seq<'U> =
            source |> toComposer |> Composer.map f |> Upcast.enumerable

        [<CompiledName "MapIndexed">]
        let mapi f source =
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
            source |> toComposer |> Composer.mapi (fun idx a ->f.Invoke(idx,a)) |> Upcast.enumerable

        [<CompiledName "MapIndexed2">]
        let mapi2 (mapfn:int->'T->'U->'V) (source1:seq<'T>) (source2:seq<'U>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let f = OptimizedClosures.FSharpFunc<int,'T,'U,'V>.Adapt mapfn
            (source1|>toComposer, source2|>toComposer)
            ||> Composer.mapi2 (fun idx a b ->f.Invoke(idx,a,b)) |> Upcast.enumerable

        [<CompiledName "Map2">]
        let map2<'T,'U,'V> (mapfn:'T->'U->'V) (source1:seq<'T>) (source2:seq<'U>) : seq<'V> =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            (source1|>toComposer, source2|>toComposer)
            ||> Composer.map2 mapfn |> Upcast.enumerable

        [<CompiledName "Map3">]
        let map3 mapfn source1 source2 source3 =
            checkNonNull "source2" source2
            checkNonNull "source3" source3
            (source1|>toComposer, source2|>toComposer, source3|>toComposer)
            |||> Composer.map3 mapfn |> Upcast.enumerable

        [<CompiledName("Choose")>]
        let choose f source =
            source |> toComposer |> Composer.choose f |> Upcast.enumerable

        [<CompiledName("Indexed")>]
        let indexed source =
            source |> toComposer |> Composer.indexed |> Upcast.enumerable

        [<CompiledName("Zip")>]
        let zip source1 source2  =
            map2 (fun x y -> x,y) source1 source2

        [<CompiledName("Zip3")>]
        let zip3 source1 source2  source3 =
            map2 (fun x (y,z) -> x,y,z) source1 (zip source2 source3)

        [<CompiledName("Cast")>]
        let cast (source: IEnumerable) =
            checkNonNull "source" source
            mkSeq (fun () -> IEnumerator.cast (source.GetEnumerator()))

        [<CompiledName("TryPick")>]
        let tryPick f (source : seq<'T>)  =
            source |> toComposer |> Composer.tryPick f

        [<CompiledName("Pick")>]
        let pick f source  =
            match tryPick f source with
            | None -> indexNotFound()
            | Some x -> x

        [<CompiledName("TryFind")>]
        let tryFind f (source : seq<'T>)  =
            source |> toComposer |> Composer.tryFind f

        [<CompiledName("Find")>]
        let find f source =
            match tryFind f source with
            | None -> indexNotFound()
            | Some x -> x

        [<CompiledName("Take")>]
        let take count (source : seq<'T>)    =
            if count < 0 then invalidArgInputMustBeNonNegative "count" count
            (* Note: don't create or dispose any IEnumerable if n = 0 *)
            if count = 0 then empty else
            source |> toComposer |> Composer.take (SR.GetString SR.notEnoughElements) count |> Upcast.enumerable

        [<CompiledName("IsEmpty")>]
        let isEmpty (source : seq<'T>)  =
            checkNonNull "source" source
            match source with
            | :? ('T[]) as a -> a.Length = 0
            | :? list<'T> as a -> a.IsEmpty
            | :? ICollection<'T> as a -> a.Count = 0
            | _ ->
                use ie = source.GetEnumerator()
                not (ie.MoveNext())

        [<CompiledName("Concat")>]
        let concat (sources:seq<#seq<'T>>) : seq<'T> =
            checkNonNull "sources" sources
            upcast Composer.Enumerable.ConcatEnumerable sources

        [<CompiledName("Length")>]
        let length (source : seq<'T>)    =
            checkNonNull "source" source
            match source with
            | :? ('T[]) as a -> a.Length
            | :? ('T list) as a -> a.Length
            | :? ICollection<'T> as a -> a.Count
            | _ ->
                use e = source.GetEnumerator()
                let mutable state = 0
                while e.MoveNext() do
                    state <-  state + 1
                state

        [<CompiledName "Fold">]
        let fold<'T,'State> (f:'State->'T->'State)  (x:'State) (source:seq<'T>) =
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            source |> toComposer
            |> Composer.fold<'T,'State>(fun (a:'State) (b:'T) -> f.Invoke(a,b)) x


        [<CompiledName "Fold2">]
        let fold2<'T1,'T2,'State> f (state:'State) (source1: seq<'T1>) (source2: seq<'T2>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            (source1 |> toComposer, source2|>toComposer)
            ||> Composer.fold2(fun s a b -> f.Invoke(s,a,b)) state

        [<CompiledName "Reduce">]
        let reduce f (source : seq<'T>)  =
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            source |> toComposer |> Composer.reduce(fun a b -> f.Invoke(a,b))

        [<CompiledName("Replicate")>]
        let replicate count x =
            #if FX_ATLEAST_40
            System.Linq.Enumerable.Repeat(x,count)
            #else
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            seq { for _ in 1 .. count -> x }
            #endif


        [<CompiledName("Append")>]
        let append (source1: seq<'T>) (source2: seq<'T>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            match source1 with
            | :? Composer.Enumerable.EnumerableBase<'T> as s -> s.Append source2
            | _ -> Upcast.enumerable (new Composer.Enumerable.AppendEnumerable<_>([source2; source1]))


        [<CompiledName("Collect")>]
        let collect f sources = map f sources |> concat

        [<CompiledName "CompareWith">]
        let compareWith (f:'T -> 'T -> int) (source1 : seq<'T>) (source2: seq<'T>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            (source1|>toComposer, source2|>toComposer)
            ||> Composer.compareWith (fun a b -> f.Invoke(a,b))


        [<CompiledName("OfList")>]
        let ofList (source : 'T list) =
            (source :> seq<'T>)

        [<CompiledName("ToList")>]
        let toList (source : seq<'T>) =
            checkNonNull "source" source
            List.ofSeq source // PATCHED

        // Create a new object to ensure underlying array may not be mutated by a backdoor cast
        [<CompiledName("OfArray")>]
        let ofArray (source : 'T array) =
            checkNonNull "source" source
            Upcast.enumerable (Composer.Array.createId source)

        [<CompiledName("ToArray")>]
        let toArray (source : seq<'T>)  =
            checkNonNull "source" source
            match source with
            | :? ('T[]) as res -> (res.Clone() :?> 'T[])
            | :? ('T list) as res -> List.toArray res
            | :? ICollection<'T> as res ->
                // Directly create an array and copy ourselves.
                // This avoids an extra copy if using ResizeArray in fallback below.
                let arr = Array.zeroCreateUnchecked res.Count
                res.CopyTo(arr, 0)
                arr
            | _ ->
                let res = ResizeArray<_>(source)
                res.ToArray()

        let foldArraySubRight (f:OptimizedClosures.FSharpFunc<'T,_,_>) (arr: 'T[]) start fin acc =
            let mutable state = acc
            for i = fin downto start do
                state <- f.Invoke(arr.[i], state)
            state

        [<CompiledName("FoldBack")>]
        let foldBack<'T,'State> f (source : seq<'T>) (x:'State) =
            checkNonNull "source" source
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let arr = toArray source
            let len = arr.Length
            foldArraySubRight f arr 0 (len - 1) x

        [<CompiledName("FoldBack2")>]
        let foldBack2<'T1,'T2,'State> f (source1 : seq<'T1>) (source2 : seq<'T2>) (x:'State) =
            let zipped = zip source1 source2
            foldBack ((<||) f) zipped x

        [<CompiledName("ReduceBack")>]
        let reduceBack f (source : seq<'T>) =
            checkNonNull "source" source
            let arr = toArray source
            match arr.Length with
            | 0 -> invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
            | len ->
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                foldArraySubRight f arr 0 (len - 2) arr.[len - 1]

        [<CompiledName("Singleton")>]
        let singleton x = mkSeq (fun () -> IEnumerator.Singleton x)


        [<CompiledName "Truncate">]
        let truncate n (source: seq<'T>) =
            if n <= 0 then empty else
            source |> toComposer |> Composer.truncate n |> Upcast.enumerable

        [<CompiledName "Pairwise">]
        let pairwise<'T> (source:seq<'T>) : seq<'T*'T> =
            source |> toComposer |> Composer.pairwise  |> Upcast.enumerable

        [<CompiledName "Scan">]
        let scan<'T,'State> (folder:'State->'T->'State) (state:'State) (source:seq<'T>) : seq<'State> =
            source |> toComposer |> Composer.scan folder state |> Upcast.enumerable

        [<CompiledName("TryFindBack")>]
        let tryFindBack f (source : seq<'T>) =
            checkNonNull "source" source
            source |> toArray |> Array.tryFindBack f

        [<CompiledName("FindBack")>]
        let findBack f source =
            checkNonNull "source" source
            source |> toArray |> Array.findBack f

        [<CompiledName("ScanBack")>]
        let scanBack<'T,'State> f (source : seq<'T>) (acc:'State) =
            checkNonNull "source" source
            mkDelayedSeq(fun () ->
                let arr = source |> toArray
                let res = Array.scanSubRight f arr 0 (arr.Length - 1) acc
                res :> seq<_>)

        [<CompiledName "TryFindIndex">]
        let tryFindIndex p (source:seq<_>) =
            source |> toComposer |> Composer.tryFindIndex p

        [<CompiledName("FindIndex")>]
        let findIndex p (source:seq<_>) =
            match tryFindIndex p source with
            | None -> indexNotFound()
            | Some x -> x

        [<CompiledName("TryFindIndexBack")>]
        let tryFindIndexBack f (source : seq<'T>) =
            checkNonNull "source" source
            source |> toArray |> Array.tryFindIndexBack f

        [<CompiledName("FindIndexBack")>]
        let findIndexBack f source =
            checkNonNull "source" source
            source |> toArray |> Array.findIndexBack f

        // windowed : int -> seq<'T> -> seq<'T[]>
        [<CompiledName("Windowed")>]
        let windowed windowSize (source: seq<_>) =
            if windowSize <= 0 then invalidArgFmt "windowSize" "{0}\nwindowSize = {1}"
                                        [|SR.GetString SR.inputMustBePositive; windowSize|]
            source |> toComposer |> Composer.windowed windowSize |> Upcast.enumerable

        [<CompiledName("Cache")>]
        let cache (source : seq<'T>) =
            checkNonNull "source" source
            // Wrap a seq to ensure that it is enumerated just once and only as far as is necessary.
            //
            // This code is required to be thread safe.
            // The necessary calls should be called at most once (include .MoveNext() = false).
            // The enumerator should be disposed (and dropped) when no longer required.
            //------
            // The state is (prefix,enumerator) with invariants:
            //   * the prefix followed by elts from the enumerator are the initial sequence.
            //   * the prefix contains only as many elements as the longest enumeration so far.
            let prefix      = ResizeArray<_>()
            let enumeratorR = ref None : IEnumerator<'T> option option ref // nested options rather than new type...
                               // None          = Unstarted.
                               // Some(Some e)  = Started.
                               // Some None     = Finished.
            let oneStepTo i =
              // If possible, step the enumeration to prefix length i (at most one step).
              // Be speculative, since this could have already happened via another thread.
              if not (i < prefix.Count) then // is a step still required?
                  // If not yet started, start it (create enumerator).
                  match !enumeratorR with
                  | None -> enumeratorR := Some (Some (source.GetEnumerator()))
                  | Some _ -> ()
                  match (!enumeratorR).Value with
                  | Some enumerator -> if enumerator.MoveNext() then
                                          prefix.Add(enumerator.Current)
                                       else
                                          enumerator.Dispose()     // Move failed, dispose enumerator,
                                          enumeratorR := Some None // drop it and record finished.
                  | None -> ()
            let result =
                unfold (fun i ->
                              // i being the next position to be returned
                              // A lock is needed over the reads to prefix.Count since the list may be being resized
                              // NOTE: we could change to a reader/writer lock here
                              lock enumeratorR (fun () ->
                                  if i < prefix.Count then
                                    Some (prefix.[i],i+1)
                                  else
                                    oneStepTo i
                                    if i < prefix.Count then
                                      Some (prefix.[i],i+1)
                                    else
                                      None)) 0
            let cleanup() =
               lock enumeratorR (fun () ->
                   prefix.Clear()
                   begin match !enumeratorR with
                   | Some (Some e) -> IEnumerator.dispose e
                   | _ -> ()
                   end
                   enumeratorR := None)
            (new CachedSeq<_>(cleanup, result) :> seq<_>)

        [<CompiledName("AllPairs")>]
        let allPairs source1 source2 =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let cached = cache source2
            source1 |> collect (fun x -> cached |> map (fun y -> x,y))

        [<CodeAnalysis.SuppressMessage("Microsoft.Naming","CA1709:IdentifiersShouldBeCasedCorrectly"); CodeAnalysis.SuppressMessage("Microsoft.Naming","CA1707:IdentifiersShouldNotContainUnderscores"); CodeAnalysis.SuppressMessage("Microsoft.Naming","CA1704:IdentifiersShouldBeSpelledCorrectly")>]
        [<CompiledName("ReadOnly")>]
        let readonly (source:seq<_>) =
            checkNonNull "source" source
            mkSeq (fun () -> source.GetEnumerator())

        let inline groupByImpl (comparer:IEqualityComparer<'SafeKey>) (keyf:'T->'SafeKey) (getKey:'SafeKey->'Key) (seq:seq<'T>) =
            checkNonNull "seq" seq

            let dict = Dictionary<_,ResizeArray<_>> comparer

            // Previously this was 1, but I think this is rather stingy, considering that we are already paying
            // for at least a key, the ResizeArray reference, which includes an array reference, an Entry in the
            // Dictionary, plus any empty space in the Dictionary of unfilled hash buckets.
            let minimumBucketSize = 4

            // Build the groupings
            seq |> iter (fun v ->
                let safeKey = keyf v
                let mutable prev = Unchecked.defaultof<_>
                match dict.TryGetValue (safeKey, &prev) with
                | true -> prev.Add v
                | false ->
                    let prev = ResizeArray ()
                    dict.[safeKey] <- prev
                    prev.Add v)

            // Trim the size of each result group, don't trim very small buckets, as excessive work, and garbage for
            // minimal gain
            dict |> iter (fun group -> if group.Value.Count > minimumBucketSize then group.Value.TrimExcess())

            // Return the sequence-of-sequences. Don't reveal the
            // internal collections: just reveal them as sequences
            dict |> map (fun group -> (getKey group.Key, readonly group.Value))

        // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tailcalls which affect performance
        let groupByValueType (keyf:'T->'Key) (seq:seq<'T>) = seq |> groupByImpl HashIdentity.Structural<'Key> keyf id

        // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
        let groupByRefType   (keyf:'T->'Key) (seq:seq<'T>) = seq |> groupByImpl RuntimeHelpers.StructBox<'Key>.Comparer (fun t -> RuntimeHelpers.StructBox (keyf t)) (fun sb -> sb.Value)

        [<CompiledName("GroupBy")>]
        let groupBy (keyf:'T->'Key) (seq:seq<'T>) =
#if FX_RESHAPED_REFLECTION
            if (typeof<'Key>).GetTypeInfo().IsValueType
#else
            if typeof<'Key>.IsValueType
#endif
                then mkDelayedSeq (fun () -> groupByValueType keyf seq)
                else mkDelayedSeq (fun () -> groupByRefType   keyf seq)

        [<CompiledName("Distinct")>]
        let distinct source =
            source |> toComposer |> Composer.distinct |> Upcast.enumerable

        [<CompiledName("DistinctBy")>]
        let distinctBy keyf source =
            source |> toComposer |> Composer.distinctBy keyf |> Upcast.enumerable

        [<CompiledName("SortBy")>]
        let sortBy keyf source =
            checkNonNull "source" source
            let delayedSort () =
                let array = source |> toArray
                Array.stableSortInPlaceBy keyf array
                array
            Upcast.enumerable (Composer.Array.createDelayedId delayedSort)

        [<CompiledName("Sort")>]
        let sort source =
            checkNonNull "source" source
            let delayedSort () =
                let array = source |> toArray
                Array.stableSortInPlace array
                array
            Upcast.enumerable (Composer.Array.createDelayedId delayedSort)

        [<CompiledName("SortWith")>]
        let sortWith f source =
            checkNonNull "source" source
            let delayedSort () =
                let array = source |> toArray
                Array.stableSortInPlaceWith f array
                array
            Upcast.enumerable (Composer.Array.createDelayedId delayedSort)

        [<CompiledName("SortByDescending")>]
        let inline sortByDescending keyf source =
            checkNonNull "source" source
            let inline compareDescending a b = compare (keyf b) (keyf a)
            sortWith compareDescending source

        [<CompiledName("SortDescending")>]
        let inline sortDescending source =
            checkNonNull "source" source
            let inline compareDescending a b = compare b a
            sortWith compareDescending source

        let inline countByImpl (comparer:IEqualityComparer<'SafeKey>) (keyf:'T->'SafeKey) (getKey:'SafeKey->'Key) (source:seq<'T>) =
            checkNonNull "source" source

            let dict = Dictionary comparer

            // Build the groupings
            source |> iter (fun v ->
                let safeKey = keyf v
                let mutable prev = Unchecked.defaultof<_>
                if dict.TryGetValue(safeKey, &prev)
                    then dict.[safeKey] <- prev + 1
                    else dict.[safeKey] <- 1)

            dict |> map (fun group -> (getKey group.Key, group.Value))

        // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tailcalls which affect performance
        let countByValueType (keyf:'T->'Key) (seq:seq<'T>) = seq |> countByImpl HashIdentity.Structural<'Key> keyf id

        // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
        let countByRefType   (keyf:'T->'Key) (seq:seq<'T>) = seq |> countByImpl RuntimeHelpers.StructBox<'Key>.Comparer (fun t -> RuntimeHelpers.StructBox (keyf t)) (fun sb -> sb.Value)

        [<CompiledName("CountBy")>]
        let countBy (keyf:'T->'Key) (source:seq<'T>) =
            checkNonNull "source" source

#if FX_RESHAPED_REFLECTION
            if (typeof<'Key>).GetTypeInfo().IsValueType
#else
            if typeof<'Key>.IsValueType
#endif
                then mkDelayedSeq (fun () -> countByValueType keyf source)
                else mkDelayedSeq (fun () -> countByRefType   keyf source)

        [<CompiledName "Sum">]
        let inline sum (source:seq<'a>) : 'a =
            source |> toComposer |> Composer.sum

        [<CompiledName "SumBy">]
        let inline sumBy (f : 'T -> ^U) (source: seq<'T>) : ^U =
            source |> toComposer |> Composer.sumBy f

        [<CompiledName "Average">]
        let inline average (source: seq< ^a>) : ^a =
            source |> toComposer |> Composer.average

        [<CompiledName "AverageBy">]
        let inline averageBy (f : 'T -> ^U) (source: seq< 'T >) : ^U =
            source |> toComposer |> Composer.averageBy f

        [<CompiledName "Min">]
        let inline min (source: seq<'T>): 'T when 'T : comparison =
            source |> toComposer |> Composer.min

        [<CompiledName "MinBy">]
        let inline minBy (projection: 'T -> 'U when 'U:comparison) (source: seq<'T>) : 'T =
            source |> toComposer |> Composer.minBy projection
(*
        [<CompiledName("MinValueBy")>]
        let inline minValBy (f : 'T -> 'U) (source: seq<'T>) : 'U =
            checkNonNull "source" source
            use e = source.GetEnumerator()
            if not (e.MoveNext()) then
                invalidArg "source" InputSequenceEmptyString
            let first = e.Current
            let mutable acc = f first
            while e.MoveNext() do
                let currv = e.Current
                let curr = f currv
                if curr < acc then
                    acc <- curr
            acc

*)
        [<CompiledName "Max">]
        let inline max (source: seq<'T>) =
            source |> toComposer |> Composer.max

        [<CompiledName "MaxBy">]
        let inline maxBy (projection: 'T -> 'U) (source: seq<'T>) : 'T =
            source |> toComposer |> Composer.maxBy projection

(*
        [<CompiledName("MaxValueBy")>]
        let inline maxValBy (f : 'T -> 'U) (source: seq<'T>) : 'U =
            checkNonNull "source" source
            use e = source.GetEnumerator()
            if not (e.MoveNext()) then
                invalidArg "source" InputSequenceEmptyString
            let first = e.Current
            let mutable acc = f first
            while e.MoveNext() do
                let currv = e.Current
                let curr = f currv
                if curr > acc then
                    acc <- curr
            acc

*)
        [<CompiledName "TakeWhile">]
        let takeWhile predicate (source: seq<_>) =
            source |> toComposer |> Composer.takeWhile predicate |> Upcast.enumerable

        [<CompiledName "SkipWhile">]
        let skipWhile predicate (source: seq<_>) =
            source |> toComposer |> Composer.skipWhile predicate |> Upcast.enumerable

        [<CompiledName "ForAll2">]
        let forall2 p (source1: seq<_>) (source2: seq<_>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let p = OptimizedClosures.FSharpFunc<_,_,_>.Adapt p
            (source1|>toComposer, source2|>toComposer)
            ||> Composer.forall2 (fun a b -> p.Invoke(a,b))

        [<CompiledName "Exists2">]
        let exists2 p (source1: seq<_>) (source2: seq<_>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            let p = OptimizedClosures.FSharpFunc<_,_,_>.Adapt p
            (source1|>toComposer, source2|>toComposer)
            ||> Composer.exists2 (fun a b -> p.Invoke(a,b))

        [<CompiledName "Head">]
        let head (source : seq<_>) =
            match tryHead source with
            | None -> invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
            | Some x -> x

        [<CompiledName "Tail">]
        let tail (source: seq<'T>) =
            source |> toComposer |> Composer.tail (SR.GetString SR.notEnoughElements) |> Upcast.enumerable

        [<CompiledName "TryLast">]
        let tryLast (source : seq<_>) =
            source |> toComposer |> Composer.tryLast

        [<CompiledName("Last")>]
        let last (source : seq<_>) =
            match tryLast source with
            | None -> invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
            | Some x -> x

        [<CompiledName "ExactlyOne">]
        let exactlyOne (source : seq<_>) =
            source |> toComposer |> Composer.exactlyOne (SR.GetString(SR.inputSequenceTooLong))

        [<CompiledName("Reverse")>]
        let rev source =
            checkNonNull "source" source
            let delayedReverse () =
                let array = source |> toArray
                Array.Reverse array
                array
            Upcast.enumerable (Composer.Array.createDelayedId delayedReverse)

        [<CompiledName("Permute")>]
        let permute f (source:seq<_>) =
            checkNonNull "source" source
            let delayedPermute () =
                source
                |> toArray
                |> Array.permute f
            Upcast.enumerable (Composer.Array.createDelayedId delayedPermute)

        [<CompiledName("MapFold")>]
        let mapFold<'T,'State,'Result> (f: 'State -> 'T -> 'Result * 'State) acc source =
            checkNonNull "source" source
            let arr,state = source |> toArray |> Array.mapFold f acc
            readonly arr, state

        [<CompiledName("MapFoldBack")>]
        let mapFoldBack<'T,'State,'Result> (f: 'T -> 'State -> 'Result * 'State) source acc =
            checkNonNull "source" source
            let array = source |> toArray
            let arr,state = Array.mapFoldBack f array acc
            readonly arr, state

        [<CompiledName "Except">]
        let except (itemsToExclude: seq<'T>) (source: seq<'T>) =
            checkNonNull "itemsToExclude" itemsToExclude
            if isEmpty itemsToExclude then source else
            source |> toComposer |> Composer.except itemsToExclude |> Upcast.enumerable

        [<CompiledName("ChunkBySize")>]
        let chunkBySize chunkSize (source : seq<_>) =
            checkNonNull "source" source
            if chunkSize <= 0 then invalidArgFmt "chunkSize" "{0}\nchunkSize = {1}"
                                    [|SR.GetString SR.inputMustBePositive; chunkSize|]
            seq { use e = source.GetEnumerator()
                  let nextChunk() =
                      let res = Array.zeroCreateUnchecked chunkSize
                      res.[0] <- e.Current
                      let i = ref 1
                      while !i < chunkSize && e.MoveNext() do
                          res.[!i] <- e.Current
                          i := !i + 1
                      if !i = chunkSize then
                          res
                      else
                          res |> Array.subUnchecked 0 !i
                  while e.MoveNext() do
                      yield nextChunk() }

        [<CompiledName("SplitInto")>]
        let splitInto count source =
            checkNonNull "source" source
            if count <= 0 then invalidArgFmt "count" "{0}\ncount = {1}"
                                [|SR.GetString SR.inputMustBePositive; count|]
            mkDelayedSeq (fun () ->
                source |> toArray |> Array.splitInto count :> seq<_>)
