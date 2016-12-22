# Performance comparison of different data pipelines in .NET

*[Full source code can be found here](https://github.com/mrange/fsharpadvent2016/tree/master/src/perfcollection)*


## Changelog

1. **2016-12-20**
  1. **New performance test** - Paul Westcott ([@manofstick](https://github.com/manofstick)) made me aware that SeqComposer has a more performant API. **SeqComposer2** uses this API.

As F# developers we all know about **Seq** that allows us to transform data using pipelines:

```fsharp
let result =
  Seq.init count id
  |> Seq.map    (fun n -> int64 n * 5L)
  |> Seq.filter (fun n -> n % 7L <> 0L)
  |> Seq.map    (fun n -> n / 11L)
  |> Seq.sum
```

There are many competing data pipelines to chose from.

0. **Imperative** - If we weren't F# developers we would use imperative patterns to implement the data pipeline
1. **Seq** - Good old **Seq** from `FSharp.Core`
2. **Linq** - **Linq** + some F# idiomatic wrappers is totally ok
3. **[Nessos.Streams](https://github.com/nessos/streams)** - The gurus at *Nessos* created a data pipeline that supports push, pull, parallelism, cancelability and so on. It's also supposed to be really fast
5. **[SeqComposer](https://github.com/Microsoft/visualfsharp/pull/1570)** - There's a PR in F# repository that intends to improve performance of **Seq**.
6. **[SeqComposer2](https://github.com/Microsoft/visualfsharp/pull/1570)** - Paul Westcott ([@manofstick](https://github.com/manofstick)) made me aware that SeqComposer has a more performant API. **SeqComposer2** uses this API.
7. **PullStream** - I implemented a simple pull (like **Seq**) data pipeline to compare against
8. **PushStream** - I implemented a simple push (like supported by **Nessos.Streams**) data pipeline to compare against
9. **PushPipe** - A variant of **PushStream** intended to reduce the overhead of creating data pipelines.
10. **[Nessos.LinqOptimizer](https://github.com/nessos/LinqOptimizer)** - While **Nessos.Streams** is cool, compared to **Nessos.LinqOptimizer** it pales as the later optimizes the data pipeline when constructed to improve performance.

I asked myself; *which data pipeline has the lowest overhead?*

To test this I created a simple pipeline that I implemented for all variants above. I was inspired by a data pipeline they use to test the **SeqComposer** [PR](https://github.com/Microsoft/visualfsharp/pull/1570).

```fsharp
let simple () =
  range count
  |> map    (fun n -> int64 n * 5L)
  |> filter (fun n -> n % 7L <> 0L)
  |> map    (fun n -> n / 11L)
  |> sum
```

1. To measure the overhead of the data pipeline itself we keep the work in the lambda expressions small.
2. As the source is a simple range and the result is a sum any observed memory pressure should be from the data pipeline.
3. In addition, I vary the collection size from 1 to 10,000,000 but keep the overall useful work the same so that we can compare the times.

So without further ado:

## Performance in Milliseconds - F# 4, .NET 4.6.2, x64

![Performance in Milliseconds - F# 4, .NET 4.6.2, x64](http://i.imgur.com/ak73l7d.png)

## Collection Count - F# 4, .NET 4.6.2, x64

![Collection Count - F# 4, .NET 4.6.2, x64](http://i.imgur.com/6Df3fS9.png)

## Interpreting the results

*Note that the axis uses logarithmic scale*

### **Imperative**

The imperative code is the fastest in general. The reason for this is that it is implemented as an efficient for loop.

Other pipelines creates intermediate objects that support constructing the pipeline, this creates memory pressure. In order to pass values down the pipeline virtual dispatch is used which is hard for the JIT:er to inline.

I implemented a few data pipelines in C++ and modern C++ compilers are able to eliminate the intermediate objects. What remains typically is an optimized loop with almost identical performance to imperative code. That's pretty cool.

### Push is faster than Pull

The push pipelines (like **Nessos.Streams**) seems to perform better in general than pull pipeline.

One of the reasons is that in a pull pipeline for each "pipe" there has to be a conditional check to see if there is more data. For push pipelines the checks are largely eliminated.

Push is also easier to implement which is nice.

### Memory overhead of data pipelines

The reason that Collection Count is higher for small collections is that I create more pipelines to keep useful work the same. If there's memory overhead from creating a pipeline this shows up for smaller collection sizes.

This inspired me to create the **PushPipe** that allows reusing the pipeline over several runs. I think my design can be improved a lot but we see that **PushPipe** is missing from the *Collection Count* because it doesn't have to recreate the pipeline. This also makes the CPU overhead lower for smaller collection sizes.

### **Nessos.LinqOptimizer**

**Nessos.LinqOptimizer** is both among the worst and among the best. The reason is that there's a huge overhead of creating the data pipeline because **Nessos.LinqOptimizer** analyzes the expression trees, optimizes them, compiles them into ILCode which is then JIT:ed.

While this gives almost "imperative" performance for large collections it also gives poor performance for small collections. I think **Nessos.LinqOptimizer** could benefit from being able to cache and reuse the pipeline and I was looking for such a thing in the API but didn't find it.

### **Seq** performance

The careful reader notices two **Seq** measurements. **Seq2** contains a modified version of `Seq.upto` that doesn't allocate objects needlessly. This improves performance by approximately 4 times and reduces the memory pressure. It's a bit of challenge to get the semantics of `upto` to be exactly as before but if I succeed this should become an F# PR.

### **SeqComposer** brings significant performance improvements

Finally, **SeqComposer** is taken from the PR that seeks to replace **Seq**. It improves **Seq** performance by roughly an order of magnitude which is a welcome improvement. The improvement is even greater if one uses the `Composer` directly.

Hope you found this interesting,

Mårten

# Appendix

## **Imperative** vs **PushPipe**

I was asked why the **Imperative** performs better than **PushPipe** (for example). For larger collections the construction of the intermediate objects shouldn't affect performance so where's the difference coming from?

The **Imperative** loop is defined like this:

```fsharp
let rec simpleLoop sum i =
  if i < count then
    let n = int64 i
    let n = n*5L
    if n % 7L <> 0L then
      let n = n / 11L
      simpleLoop (sum + n) (i + 1)
    else
      simpleLoop sum (i + 1)
  else
    sum
let simple () =
  simpleLoop 0L 0
```

If one disassemble the JIT:ed code it looks like this:

```asm
; if i < count then
00007FFD909330C4  cmp         esi,ebx
00007FFD909330C6  jge         00007FFD90933123
; let n = int64 i
00007FFD909330C8  movsxd      rax,esi
; let n = n*5L
00007FFD909330CB  imul        rcx,rax,5
; if n % 7L <> 0L then
;   Lots of extra instructions to avoid the division
00007FFD909330CF  mov         rax,4924924924924925h
00007FFD909330D9  imul        rcx
00007FFD909330DC  mov         rax,rdx
00007FFD909330DF  sar         rax,1
00007FFD909330E2  mov         rdx,rax
00007FFD909330E5  shr         rdx,3Fh
00007FFD909330E9  add         rax,rdx
00007FFD909330EC  imul        rax,rax,7
00007FFD909330F0  mov         rdx,rcx
00007FFD909330F3  sub         rdx,rax
00007FFD909330F6  test        rdx,rdx
00007FFD909330F9  je          00007FFD9093311F
; let n = n / 11L
;   Lots of extra instructions to avoid the division
00007FFD909330FB  mov         rax,2E8BA2E8BA2E8BA3h
00007FFD90933105  imul        rcx
00007FFD90933108  mov         rax,rdx
00007FFD9093310B  sar         rax,1
00007FFD9093310E  mov         rdx,rax
00007FFD90933111  shr         rdx,3Fh
00007FFD90933115  add         rax,rdx
; simpleLoop (sum + n) (i + 1)
00007FFD90933118  inc         esi
00007FFD9093311A  add         rdi,rax
00007FFD9093311D  jmp         00007FFD909330C4
; simpleLoop sum (i + 1)
00007FFD9093311F  inc         esi
00007FFD90933121  jmp         00007FFD909330C4
```

This is basically an efficient loop implement x64 with some tricks to avoid division (expensive).

Let's compare it with the **PushPipe**:

```fsharp
  let createTestCases count =
    let inline range  c   = Stream.range 0 1 (c - 1)
    let inline filter f s = Stream.filter f s
    let inline map    m s = Stream.map    m s
    let inline sum      s = Stream.sum    s
    let simple () =
      range count
      |> map    (fun n -> int64 n * 5L)
      |> filter (fun n -> n % 7L <> 0L)
      |> map    (fun n -> n / 11L)
      |> sum
    simple
```

The JIT:ed looks like this:

```asm
; range count
; if i <= e
00007ffd`909535d7 3bf3            cmp     esi,ebx
00007ffd`909535d9 7f20            jg      00007ffd`909535fb
; if r i
00007ffd`909535db 488bcf          mov     rcx,rdi
00007ffd`909535de 8bd6            mov     edx,esi
; Load MethodTable
00007ffd`909535e0 488b07          mov     rax,qword ptr [rdi]
; Load vtable
00007ffd`909535e3 488b4040        mov     rax,qword ptr [rax+40h]
; Call virtual function (map (fun n -> int64 n * 5L))
00007ffd`909535e7 ff5020          call    qword ptr [rax+20h]
00007ffd`909535ea 84c0            test    al,al
00007ffd`909535ec 7404            je      00007ffd`909535f2
; rangeForward s e r (i + s)
00007ffd`909535ee 03f5            add     esi,ebp
00007ffd`909535f0 ebe0            jmp     00007ffd`909535d7

; map (fun n -> int64 n * 5L)
; int64 n
00007ffd`90953620 4863d2          movsxd  rdx,edx
; *5L
00007ffd`90953623 486bd205        imul    rdx,rdx,5
; Load this
00007ffd`90953627 488b4908        mov     rcx,qword ptr [rcx+8]
; Load MethodTable
00007ffd`9095362b 488b01          mov     rax,qword ptr [rcx]
; Load vtable
00007ffd`9095362e 488b4040        mov     rax,qword ptr [rax+40h]
; Load virtual function
00007ffd`90953632 488b4020        mov     rax,qword ptr [rax+20h]
; Tail call virtual function (filter (fun n -> n % 7L <> 0L))
00007ffd`90953636 48ffe0          jmp     rax

; filter (fun n -> n % 7L <> 0L)
; if n % 7L <> 0L then
;   Lots of extra instructions to avoid the division
00007ffd`90953650 4c8bc2          mov     r8,rdx
00007ffd`90953653 48b82549922449922449 mov rax,4924924924924925h
00007ffd`9095365d 49f7e8          imul    r8
00007ffd`90953660 488bc2          mov     rax,rdx
00007ffd`90953663 488bd0          mov     rdx,rax
00007ffd`90953666 48d1fa          sar     rdx,1
00007ffd`90953669 488bc2          mov     rax,rdx
00007ffd`9095366c 48c1e83f        shr     rax,3Fh
00007ffd`90953670 4803d0          add     rdx,rax
00007ffd`90953673 486bd207        imul    rdx,rdx,7
00007ffd`90953677 498bc0          mov     rax,r8
00007ffd`9095367a 482bc2          sub     rax,rdx
; Test condition
00007ffd`9095367d 4885c0          test    rax,rax
00007ffd`90953680 7415            je      00007ffd`90953697
; True case
; Load this
00007ffd`90953682 488b4908        mov     rcx,qword ptr [rcx+8]
00007ffd`90953686 498bd0          mov     rdx,r8
; Load MethodTable
00007ffd`90953689 488b01          mov     rax,qword ptr [rcx]
; Load vtable
00007ffd`9095368c 488b4040        mov     rax,qword ptr [rax+40h]
; Load virtual function
00007ffd`90953690 488b4020        mov     rax,qword ptr [rax+20h]
; Tail call virtual function (map (fun n -> n / 11L))
00007ffd`90953694 48ffe0          jmp     rax
; False case
; Return to range count (returns true to continue iterating)
00007ffd`90953697 b801000000      mov     eax,1
00007ffd`9095369c c3              ret

; map (fun n -> n / 11L)
;   Lots of extra instructions to avoid the divide
; Load this
00007ffd`909347a0 488b4908        mov     rcx,qword ptr [rcx+8]
00007ffd`909347a4 48b8a38b2ebae8a28b2e mov rax,2E8BA2E8BA2E8BA3h
00007ffd`909347ae 48f7ea          imul    rdx
00007ffd`909347b1 488bc2          mov     rax,rdx
00007ffd`909347b4 488bd0          mov     rdx,rax
00007ffd`909347b7 48d1fa          sar     rdx,1
00007ffd`909347ba 488bc2          mov     rax,rdx
00007ffd`909347bd 48c1e83f        shr     rax,3Fh
00007ffd`909347c1 4803d0          add     rdx,rax
; Load MethodTable
00007ffd`909347c4 488b01          mov     rax,qword ptr [rcx]
; Load vtable
00007ffd`909347c7 488b4040        mov     rax,qword ptr [rax+40h]
; Load virtual function
00007ffd`909347cb 488b4020        mov     rax,qword ptr [rax+20h]
; Tail call virtual function (sum)
00007ffd`909347cf 48ffe0          jmp     rax

; sum
; Load ref acc
00007ffd`909347f0 488b4108        mov     rax,qword ptr [rcx+8]
00007ffd`909347f4 488bc8          mov     rcx,rax
; Load acc
00007ffd`909347f7 488b4008        mov     rax,qword ptr [rax+8]
; Increment acc
00007ffd`909347fb 4803c2          add     rax,rdx
; Store increment
00007ffd`909347fe 48894108        mov     qword ptr [rcx+8],rax
; Return to range count (returns true to continue iterating)
00007ffd`90934802 b801000000      mov     eax,1
00007ffd`90934807 c3              ret
```

There are lot of similarities with the **Imperative** code but we see virtual tail calls in the JIT:ed code. The reason for this is this.

A `PushPipe<'T>` is defined like this

```fsharp
type Receiver<'T> = 'T            -> bool

/// <summary>The type of the push stream.</summary>
type Stream<'T>   = Receiver<'T>  -> unit
```

`Stream<'T>` is used to build up a chain of `Receiver<'T>`. Each value in the stream passed to the next receiver using virtual dispatch. These are the virtual dispatch we see in the JIT:ed code for **PushPipe**.

In principle the F# compiler could eliminate the virtual tail calls but currently it doesn't. The JIT:er doesn't have enough information nor time to inline the virtual calls.

This is the reason that **Imperative** performs better than **PushPipe**.



