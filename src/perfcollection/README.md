# Performance comparison of different data pipelines in .NET

*[Full source code can be found here](https://github.com/mrange/fsharpadvent2016/tree/master/src/perfcollection)*


## Changelog

1. **2016-12-20**
  1. **New performance test** - Paul Westcott ([@manofstick](https://github.com/manofstick)) made me aware that SeqComposer has a more performant API. **SeqComposer2** uses this API.
1. **2016-12-23**
  1. **Analysis of performance difference** - Added an appendix with an analysis to explain the performance difference between **Imperative** and **PushPipe**.

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
; n % 7L <> 0L?
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

This is an for-loop with some clever tricks to avoid expensive divisions.

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
; i <= e?
00007ffd`909535d7 3bf3            cmp     esi,ebx
00007ffd`909535d9 7f20            jg      00007ffd`909535fb
; r i
00007ffd`909535db 488bcf          mov     rcx,rdi
00007ffd`909535de 8bd6            mov     edx,esi
; Load MethodTable
00007ffd`909535e0 488b07          mov     rax,qword ptr [rdi]
; Load vtable
00007ffd`909535e3 488b4040        mov     rax,qword ptr [rax+40h]
; Call virtual function (map (fun n -> int64 n * 5L))
00007ffd`909535e7 ff5020          call    qword ptr [rax+20h]
; r i?
00007ffd`909535ea 84c0            test    al,al
00007ffd`909535ec 7404            je      00007ffd`909535f2
; True branch
; i + s and loop
00007ffd`909535ee 03f5            add     esi,ebp
00007ffd`909535f0 ebe0            jmp     00007ffd`909535d7

; map (fun n -> int64 n * 5L)
; int64 n
00007ffd`90953620 4863d2          movsxd  rdx,edx
; *5L
00007ffd`90953623 486bd205        imul    rdx,rdx,5
; Load (filter (fun n -> n % 7L <> 0L)) function object
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
; n % 7L <> 0L?
00007ffd`9095367d 4885c0          test    rax,rax
00007ffd`90953680 7415            je      00007ffd`90953697
; True branch
; Load (map (fun n -> n / 11L)) function object
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
; False branch
; Return to range count (returns true to continue iterating)
00007ffd`90953697 b801000000      mov     eax,1
00007ffd`9095369c c3              ret

; map (fun n -> n / 11L)
;   Lots of extra instructions to avoid the divide
; Load function object to next step
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

In principle the F# compiler could eliminate the virtual tail calls but currently it doesn't. Unfortunately, the JIT:er neither have enough information nor time to inline the virtual calls.

This is the reason that **Imperative** performs better than **PushPipe**.

We can do a similar analysis for **SeqComparer2**:

```asm
; Composer.init c id
;   outOfBand.HaltedIdx = 0?
00007ffd`90955d8b 837e0c00        cmp     dword ptr [rsi+0Ch],0
00007ffd`90955d8f 754e            jne     00007ffd`90955ddf
; idx < terminatingIdx?
00007ffd`90955d91 3b6b10          cmp     ebp,dword ptr [rbx+10h]
00007ffd`90955d94 0f9cc1          setl    cl
00007ffd`90955d97 0fb6c9          movzx   ecx,cl
00007ffd`90955d9a 85c9            test    ecx,ecx
00007ffd`90955d9c 7441            je      00007ffd`90955ddf
00007ffd`90955d9e 4585ff          test    r15d,r15d
; maybeSkipping?
00007ffd`90955da1 7413            je      00007ffd`90955db6
00007ffd`90955da3 498bce          mov     rcx,r14
00007ffd`90955da6 33d2            xor     edx,edx
; Load MethodTable
00007ffd`90955da8 498b06          mov     rax,qword ptr [r14]
; Load vtable
00007ffd`90955dab 488b4040        mov     rax,qword ptr [rax+40h]
; Call virtual function (isSkipping ())
00007ffd`90955daf ff5020          call    qword ptr [rax+20h]
00007ffd`90955db2 440fb6f8        movzx   r15d,al
; not maybeSkipping?
00007ffd`90955db6 4585ff          test    r15d,r15d
00007ffd`90955db9 7520            jne     00007ffd`90955ddb
; Load id function object
00007ffd`90955dbb 488b4b08        mov     rcx,qword ptr [rbx+8]
00007ffd`90955dbf 8d5501          lea     edx,[rbp+1]
; Load MethodTable
00007ffd`90955dc2 488b01          mov     rax,qword ptr [rcx]
; Load vtable
00007ffd`90955dc5 488b4040        mov     rax,qword ptr [rax+40h]
; Call virtual function (id)
00007ffd`90955dc9 ff5020          call    qword ptr [rax+20h]
00007ffd`90955dcc 8bd0            mov     edx,eax
00007ffd`90955dce 488bcf          mov     rcx,rdi
; Load (map (fun n -> int64 n * 5L)) function object
00007ffd`90955dd1 488b07          mov     rax,qword ptr [rdi]
; Load MethodTable
00007ffd`90955dd4 488b4040        mov     rax,qword ptr [rax+40h]
; Call virtual function (map (fun n -> int64 n * 5L))
00007ffd`90955dd8 ff5030          call    qword ptr [rax+30h]
; idx <- idx + 1
00007ffd`90955ddb ffc5            inc     ebp
00007ffd`90955ddd ebac            jmp     00007ffd`90955d8b

; map    (fun n -> int64 n * 5L)
; int64 n
00007ffd`90955ed0 4863d2          movsxd  rdx,edx
; * 5L
00007ffd`90955ed3 486bd205        imul    rdx,rdx,5
; Load (filter (fun n -> n % 7L <> 0L)) function object
00007ffd`90955ed7 488b4918        mov     rcx,qword ptr [rcx+18h]
; Load MethodTable
00007ffd`90955edb 488b01          mov     rax,qword ptr [rcx]
; Load vtable
00007ffd`90955ede 488b4040        mov     rax,qword ptr [rax+40h]
; Load virtual function
00007ffd`90955ee2 488b4030        mov     rax,qword ptr [rax+30h]
; Tail call virtual function (filter (fun n -> n % 7L <> 0L))
00007ffd`90955ee6 48ffe0          jmp     rax

; filter (fun n -> n % 7L <> 0L)
;   Lots of instructions to avoid the division
00007ffd`90955f00 4c8bc2          mov     r8,rdx
00007ffd`90955f03 48b82549922449922449 mov rax,4924924924924925h
00007ffd`90955f0d 49f7e8          imul    r8
00007ffd`90955f10 488bc2          mov     rax,rdx
00007ffd`90955f13 488bd0          mov     rdx,rax
00007ffd`90955f16 48d1fa          sar     rdx,1
00007ffd`90955f19 488bc2          mov     rax,rdx
00007ffd`90955f1c 48c1e83f        shr     rax,3Fh
00007ffd`90955f20 4803d0          add     rdx,rax
00007ffd`90955f23 486bd207        imul    rdx,rdx,7
00007ffd`90955f27 498bc0          mov     rax,r8
00007ffd`90955f2a 482bc2          sub     rax,rdx
; n % 7L <> 0L?
00007ffd`90955f2d 4885c0          test    rax,rax
00007ffd`90955f30 7415            je      00007ffd`90955f47
; Load (map    (fun n -> n / 11L)) function object
00007ffd`90955f32 488b4918        mov     rcx,qword ptr [rcx+18h]
00007ffd`90955f36 498bd0          mov     rdx,r8
; Load MethodTable
00007ffd`90955f39 488b01          mov     rax,qword ptr [rcx]
; Load vtable
00007ffd`90955f3c 488b4040        mov     rax,qword ptr [rax+40h]
; Load virtual function
00007ffd`90955f40 488b4030        mov     rax,qword ptr [rax+30h]
; Tail call virtual function (map    (fun n -> n / 11L))
00007ffd`90955f44 48ffe0          jmp     rax
; Set rax to 0 to indicate we like to continue
00007ffd`90955f47 33c0            xor     eax,eax
00007ffd`90955f49 c3              ret

; map    (fun n -> n / 11L)
;   Lots of instructions to avoid the division
00007ffd`90957145 48b8a38b2ebae8a28b2e mov rax,2E8BA2E8BA2E8BA3h
00007ffd`9095714f 48f7ee          imul    rsi
00007ffd`90957152 488bc2          mov     rax,rdx
00007ffd`90957155 488bc8          mov     rcx,rax
00007ffd`90957158 48d1f9          sar     rcx,1
00007ffd`9095715b 488bd1          mov     rdx,rcx
00007ffd`9095715e 48c1ea3f        shr     rdx,3Fh
00007ffd`90957162 4803ca          add     rcx,rdx
00007ffd`90957165 488bd1          mov     rdx,rcx
00007ffd`90957168 488bcf          mov     rcx,rdi
; Load MethodTable
00007ffd`9095716b 488b07          mov     rax,qword ptr [rdi]
; Load vtable
00007ffd`9095716e 488b4040        mov     rax,qword ptr [rax+40h]
; Load virtual function
00007ffd`90957172 488b4030        mov     rax,qword ptr [rax+30h]
; Restore stack and rsi, rdi
00007ffd`90957176 4883c428        add     rsp,28h
00007ffd`9095717a 5e              pop     rsi
00007ffd`9095717b 5f              pop     rdi
; Tail call virtual function (sum)
00007ffd`9095717c 48ffe0          jmp     rax

; Sum
; Allocate stack (for checked addition?)
00007ffd`909571a0 4883ec28        sub     rsp,28h
; Load acc
00007ffd`909571a4 488b4110        mov     rax,qword ptr [rcx+10h]
; acc <- acc + v
00007ffd`909571a8 4803c2          add     rax,rdx
; Overflowed? (SeqComparer uses Checked additions)
00007ffd`909571ab 700b            jo      00007ffd`909571b8
; Store acc
00007ffd`909571ad 48894110        mov     qword ptr [rcx+10h],rax
; We like to continue
00007ffd`909571b1 33c0            xor     eax,eax
; Restore stack
00007ffd`909571b3 4883c428        add     rsp,28h
00007ffd`909571b7 c3              ret
```

There are a lot of similarities with `PushStream<'T>` so even though we haven't looked at the `SeqComposer` code we see that it does support push. `SeqComposer` is more advanced than `PushStream<'T>` in that it supports pull as well.

The difference in performance between **PushStream** and **SeqComposer2** seems to come mostly from a more generic top-loop:

`PushStream<'T>` has a specialized loop for `range`:

```fsharp
let rec rangeForward s e r i = if i <= e then if r i then rangeForward s e r (i + s)
```

For `SeqComposer` we use `Composer.init c id` to implement `range`:

```fsharp
while (outOfBand.HaltedIdx = 0) && (idx < terminatingIdx) do
    if maybeSkipping then
        maybeSkipping <- isSkipping ()

    if not maybeSkipping then
        consumer.ProcessNext (f (idx+1)) |> ignore

    idx <- idx + 1
```

It seems the difference in performance mainly comes from that `SeqComposer` has 2 extra checks per value and an extra virtual call (`isSkipping ()` is not called in the pipeline above). `SeqComposer` also uses checked addition which adds a small overhead (a conditional branch and stack adjusting).

