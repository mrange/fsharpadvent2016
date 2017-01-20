module PerformanceTests =
  open FSharp.Core.Printf

  open System
  open System.Collections
  open System.Diagnostics
  open System.IO
  open System.Text
  open System.Threading.Tasks

  // now () returns current time in milliseconds since start
  let now : unit -> int64 =
    let sw = Stopwatch ()
    sw.Start ()
    fun () -> sw.ElapsedMilliseconds

  // time estimates the time 'action' repeated a number of times
  let time repeat action =
    let inline cc i       = System.GC.CollectionCount i

    let v                 = action ()

    System.GC.Collect (2, System.GCCollectionMode.Forced, true)

    let bcc0, bcc1, bcc2  = cc 0, cc 1, cc 2
    let b                 = now ()

    for i in 1..repeat do
      action () |> ignore

    let e = now ()
    let ecc0, ecc1, ecc2  = cc 0, cc 1, cc 2

    v, (e - b), ecc0 - bcc0, ecc1 - bcc1, ecc2 - bcc2

  let inline dbreak () = System.Diagnostics.Debugger.Break ()

  type TestResult =  TestResult of string*string*string*int64*int*int*int
  let testResult t y x tm cc0 cc1 cc2 = TestResult (t, y, x, tm, cc0, cc1, cc2)
  let testClass (TestResult (t, _, _, _, _, _, _))  = t
  let testCase  (TestResult (_, tc, _, _, _, _, _)) = tc
  let testX     (TestResult (_, _, x, _, _, _, _))  = x

  module Random =
    let create (seed : int) =
      let mutable state = int64 seed
      let m = 0x7FFFFFFFL // 2^31 - 1
      let d = 1. / float m
      let a = 48271L      // MINSTD
      let c = 0L
      fun () ->
        state <- (a*state + c) % m
        float state * d

    let range random b e =
      let r = random ()
      let v = float (e - b)*r + float b |> int
      v

  module Array2DPerf =
    let createTestCases (a : float[,]) (b : float[,]) =

      let a1 = Array2D.length1 a
      let a2 = Array2D.length2 a
      let b1 = Array2D.length1 b
      let b2 = Array2D.length2 b

      let r = Array2D.zeroCreate a1 b2

      if a2 <> b1 then
        failwithf "Wrong dimensions on input %dx%d * %dx%d" a1 a2 b1 b2

      let inline sum (a : float[,]) = 
#if DEBUG
        let mutable sum = 0.
        for v in a do
          sum <- sum + (v :?> float)
          
        sum
#else
        0.
#endif

      let naiveMultiply () =
        for row = 0 to (a1 - 1) do
          for col = 0 to (b2 - 1) do
            let mutable sum = 0.
            for i = 0 to (a2 - 1) do
              sum <- sum + a.[row, i]*b.[i, col]
            r.[row, col] <- sum

        sum r

      let betterMultiply () =
        for row = 0 to (a1 - 1) do
          for col = 0 to (b2 - 1) do
            r.[row, col] <- 0.

        for row = 0 to (a1 - 1) do
          for i = 0 to (a2 - 1) do
            let av = a.[row, i]
            for col = 0 to (b2 - 1) do
              r.[row, col] <- r.[row, col] + av*b.[i, col]

        sum r

      naiveMultiply, betterMultiply
      
  module ArrayJaggedPerf =
    let createTestCases (a : float[,]) (b : float[,]) =

      let a1 = Array2D.length1 a
      let a2 = Array2D.length2 a
      let b1 = Array2D.length1 b
      let b2 = Array2D.length2 b

      if a2 <> b1 then
        failwithf "Wrong dimensions on input %dx%d * %dx%d" a1 a2 b1 b2

      let toJagged a = 
        let a1 = Array2D.length1 a
        let a2 = Array2D.length2 a
        [| for row = 0 to (a1 - 1) do yield [| for col = 0 to (a2 - 1) do yield a.[row, col] |] |]

      let a  = toJagged a
      let b  = toJagged b

      let r = [| for row = 0 to (a1 - 1) do yield [| for col = 0 to (b2 - 1) do yield 0. |] |]

      let inline sum a = 
#if DEBUG
        a |> Seq.collect id |> Seq.sum
#else
        0.
#endif

      let naiveMultiply () =
        for row = 0 to (a1 - 1) do
          for col = 0 to (b2 - 1) do
            let mutable sum = 0.
            let ar = a.[row]
            for i = 0 to (a2 - 1) do
              sum <- sum + ar.[i]*b.[i].[col]
            r.[row].[col] <- sum

        sum r

      let betterMultiply () =
        for row = 0 to (a1 - 1) do
          let rr = r.[row]
          System.Array.Clear (rr, 0, rr.Length)

        for row = 0 to (a1 - 1) do
          for i = 0 to (a2 - 1) do
            let mutable sum = 0.
            let av = a.[row].[i]
            let br = b.[i]
            let rr = r.[row]
            for col = 0 to (b2 - 1) do
              rr.[col] <- rr.[col] + av*br.[col]

        sum r

      naiveMultiply, betterMultiply
      
  module Array1DPerf =
    let createTestCases (a : float[,]) (b : float[,]) =

      let a1 = Array2D.length1 a
      let a2 = Array2D.length2 a
      let b1 = Array2D.length1 b
      let b2 = Array2D.length2 b

      if a2 <> b1 then
        failwithf "Wrong dimensions on input %dx%d * %dx%d" a1 a2 b1 b2

      let to1D a = 
        let a1 = Array2D.length1 a
        let a2 = Array2D.length2 a
        [| for row = 0 to (a1 - 1) do for col = 0 to (a2 - 1) do yield a.[row, col] |]

      let a  = to1D a
      let b  = to1D b

      let r = Array.zeroCreate (a1*b2)

      let inline sum a = 
#if DEBUG
        a |> Array.sum
#else
        0.
#endif

      let naiveMultiply () =
        for row = 0 to (a1 - 1) do
          let ro = row * a2
          for col = 0 to (b2 - 1) do
            let mutable sum = 0.
            for i = 0 to (a2 - 1) do
              sum <- sum + a.[ro + i]*b.[i * b2 + col]
            r.[row*b2 + col] <- sum

        sum r

      let betterMultiply () =
        System.Array.Clear (r, 0, r.Length)

        for row = 0 to (a1 - 1) do
          let ao = row * a2
          let ro = row * b2
          for i = 0 to (a2 - 1) do
            let av = a.[ao + i]
            let bo = i * b2
            for col = 0 to (b2 - 1) do
              let ri = ro + col
              r.[ri] <- r.[ri] + av*b.[bo + col]

        sum r

      naiveMultiply, betterMultiply
      

  let run () =
#if DEBUG
    let count   = 1000000
#else
    let count   = 1000000000
#endif
    let dims  =
#if DEBUG
      [|
        10  , 10
        10    , 100
        100   , 10
        100   , 100
      |]
#else
      [|
        10    , 10
        10    , 100
        100   , 10
        100   , 100
        100   , 1000
        1000  , 100
        1000  , 1000
      |]
#endif

    let testCases =
      [|
        "Array2D"       , Array2DPerf.createTestCases
        "ArrayJagged"   , ArrayJaggedPerf.createTestCases
        "Array1D"       , Array1DPerf.createTestCases
      |]

    let random = Random.create 19740531

    let results = ResizeArray 16

    for dimr, dimc in dims do

      let inner = dimr * dimc * dimr
      let outer = count / inner

      printfn "Running test cases with outer=%d, inner=%d"  outer inner

      printfn "    Creating matrices with dim %dx%d" dimr dimc

      let random = Random.create 19740531

      let a = Array2D.init dimr dimc (fun _ _ -> random ())
      let b = Array2D.init dimc dimr (fun _ _ -> random ())

      for name, creator in testCases do
        printfn "  Test case %A" name

        let result t a =
          printfn "    Runnning test case: %s" t
          let v, time, cc0, cc1, cc2 = time outer a
          results.Add <| testResult t name (sprintf "%dx%d" dimr dimc) time cc0 cc1 cc2
          printfn "      = %A" v

        printfn "    Creating test cases"
        let naiveMultiply, betterMultiply = creator a b

        result "NaiveMultiply"  naiveMultiply
        result "BetterMultiply" betterMultiply

    let results = results.ToArray ()

    let testXs      = results |> Array.groupBy testX |> Array.map fst
    let testCases   = results |> Array.groupBy testCase
    let testClasses = results |> Array.groupBy testClass

    let header  = "Name" + (testXs |> Array.map (fun i -> ",'" + string i) |> Array.reduce (+))

    for testClassName, _ in testClasses do
      use perf      = new StreamWriter ("perf_" + testClassName + ".csv")
      use cc        = new StreamWriter ("cc_"   + testClassName + ".csv")
      let line sw l = (sw : StreamWriter).WriteLine (l : string)
      let linef sw f= kprintf (line sw) f

      line perf header
      line cc   header

      for testCaseName, testCaseResults in testCases do
        let write sb s  = (sb : StringBuilder).Append (s : string) |> ignore
        let field sb s  = (sb : StringBuilder).Append ',' |> ignore; write sb s
        let fieldf sb f = kprintf (field sb) f
        let psb         = StringBuilder 16
        let csb         = StringBuilder 16
        write psb testCaseName
        write csb testCaseName
        let m = testCaseResults |> Array.map (fun tr -> (testClass tr, testX tr), tr) |> Map.ofArray
        for testX in testXs do
          let (TestResult (_, _, _, tm, cc0 ,_, _)) = m.[testClassName, testX]
          fieldf psb "%d" tm
          fieldf csb "%d" cc0

        line perf <| psb.ToString ()
        line cc   <| csb.ToString ()

open System

[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
    PerformanceTests.run ()
    0
  with
  | e ->
    printfn "Caught: %s" e.Message
    999
