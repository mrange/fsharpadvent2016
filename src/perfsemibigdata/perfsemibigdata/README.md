# On the topic of data locality and performance

*[Full source code can be found here](https://github.com/mrange/fsharpadvent2016/tree/master/src/perfsemibigdata)*

It is well-known that a hard disk has a long delay from that we request the data to that we get the data. Usually we measure the hard disk latency in milliseconds which is an eternity for a CPU. The bandwidth of a hard disk is decent good as SSD:s today can reach 1 GiB/second.

What is less known is that RAM has the same characteristics, bad latency with good bandwidth.

You can measure RAM latency and badndwidth using [Intel® Memory Latency Checker](https://software.intel.com/en-us/articles/intelr-memory-latency-checker). On my machine the RAM latency under semi-high load is ~120 ns (The `3r:1w` bandwidth is 16GiB/second). This means that the CPU on my machine has to wait for ~400 cycles for data, an eternity.

To "fix" the latency problem over the years CPUs has accumulated more and more cache as well as advanced techniques to schedule reads earlier and delaying writes to avoid starving for data. So much in fact that only ~1% of the CPU die is used for computation and the rest is used to fix the problem of slow RAM. In addition, because bandwidth is good and latency is poor CPUs reads more data than what the CPU requested (prefetching).

The CPU on my machine has 3 layers of cache:

```
L1: 256KiB
L2: 1MiB
L3: 6MiB
```

A simple way of thinking about the cost of reading data is that if reading from L1 has the cost of `1`, the cost of reading from L2 is `10`, the cost of reading from L3 is `100` and the cost of reading from RAM is `1000`.

When we need to write highly performant code that process a lot of data we want to minimize the foot-print of the data to ensure as much as possible fits into the L1 cache and that we uses the band-width as efficiently as possible. Because of the prefetcher we should read data sequentially.

## What is a lot of data?

What lot of data means depends on the context. Sometimes it's 16KiB, sometimes it's 16TiB but as the L3 cache on my machine is 6MiB any data set bigger than 6MiB is considered "big" in my examples.

## How do we make sure we read the data sequentially?

One of the fundamental principles of .NET (and Java) is that memory is managed. .NET prevents developers from unintentionally corrupting data and has a GC that can defragment the heap for us. .NET also hides the location of the memory for us as it's not important for the correctness of a program. The location is important when we need to ensure we access memory sequentially for performance.

One of the simplest ways to access data sequentially is to use arrays. Arrays are a continuous block of memory, if we traverse it from `0..last` we access the array data sequentially.

The actual data in an array is often a reference to an object which may or may not lay sequential in memory. The GC tries to preserve the order of the objects in the way they were created but it's not certain that correlates to how you will access the data.

.NET allows us to create arrays of `struct`. A `struct` in .NET is a value object which means that when we iterate an array of `struct` we are indeed accessing the data sequentially.

## Measuring the cost of data access

In order to measure different data access schemes, I have created a few test cases that uses various patterns to access data.

The basic premise that we have a large set of particles that contain:

1. Mass
2. Current position
3. Previous position
4. Hot Data (data that would be accessed often)
5. Cold Data (data that would be accessed seldom)

We will perform something called verlet integration over all particles, this is common in game physics. It's a very simple computation

```fsharp
  let newPos  =  currentPos + currentPos - previousPos + globalAcceleration
  previousPos <- currentPos
  currentPos  <- newPos
```

This computation only access current and previous position and doesn't look at neither the hot data or the cold data.

We obviously like the verlet integration to be as fast as possible.

With the mass, current and previous position, hot data and cold data a particle takes 216 bytes. We will have up to 1,000,000 particles which means the particles will take 216,000,0000 bytes which is way more than my 6MiB of L3 cache. Therefore we expect that the way we access data will have an impact.

### The `Particle` class

For the  test cases **Class** and **Class (Shuffle)** we create a `Particle` class like this:

```fsharp
    [<NoComparison>]
    [<NoEquality>]
    type Particle =
      class
        val mutable mass      : float
        val mutable current   : Vector
        val mutable previous  : Vector
        val mutable hot       : HotData
        val mutable cold      : ColdData

        new (mass, current, previous) = { mass = mass; current = current; previous = previous; hot = HotData (); cold = ColdData () }

        member x.Mass     = x.mass
        member x.Position = x.current

        member x.Verlet globalAcceleration =
          let next    =  x.current + x.current - x.previous + globalAcceleration
          x.previous  <- x.current
          x.current   <- next

        static member New timeStep mass position velocity =
          let current   = position
          let previous  = position - (timeStep * (velocity : Vector))
          Particle (mass, current, previous)

      end
```

We then create two arrays.

1. For test case **Class** we iterate over the first array where all object references in the order the objects were created. This mean we access the particle data sequentially when we iterate the array.
2. For test **Class (Shuffle)** we iterate over the second array where all object references are randomly shuffled. This mean we access the particle data randomly when we iterate the array.*Note: if you store particles in a dictionary and iterate it the order is effectively random as well*

For small sets of data we expect no difference in performance when iterating the arrays as the data fits into L1. When the data sets grow we are expecting **Class** to perform better because we make better use of the prefetcher.

### The `Particle` struct

For the test case **Struct** and **Struct (Shuffle)** we create a `Particle` struct like this:

```fsharp
    [<NoComparison>]
    [<NoEquality>]
    type Particle =
      struct
        val mutable mass      : float
        val mutable current   : Vector
        val mutable previous  : Vector
        val mutable hot       : HotData
        val mutable cold      : ColdData

        new (mass, current, previous) = { mass = mass; current = current; previous = previous; hot = HotData(); cold = ColdData () }

        member x.Mass     = x.mass
        member x.Position = x.current

        member x.Verlet globalAcceleration =
          let next    =  x.current + x.current - x.previous + globalAcceleration
          x.previous  <- x.current
          x.current   <- next

        static member New timeStep mass position velocity =
          let current   = position
          let previous  = position - (timeStep * (velocity : Vector))
          Particle (mass, current, previous)

      end
```

The only difference is that we use `struct` over `class`. We create two arrays like before, one in order of creation, the other shuffled. We expect no difference in performance between these two as `struct` should guarantee that always access data sequentially.

### Separate Hot & Cold data

For test case **Hot & Cold** in our fictitious program we have identified hot data (data that is accessed often) that is grouped together, cold data (accessed more seldomly) is put into a different group. For this particular algorithm we only need to access the hot data.

This is common approach to make sure we don't waste memory bandwidth on loading cold data which we don't access during verlet integration. We still have some overhead as there are hot data the verlet interation doesn't need but that are needed for the constraint relaxation.

It looks like this:

```fsharp
    [<NoComparison>]
    [<NoEquality>]
    type Particle =
      struct
        val mutable mass      : float
        val mutable current   : Vector
        val mutable previous  : Vector
        val mutable hot       : HotData

        new (mass, current, previous) = { mass = mass; current = current; previous = previous; hot = HotData(); }

        member x.Mass     = x.mass
        member x.Position = x.current

        member x.Verlet globalAcceleration =
          let next    =  x.current + x.current - x.previous + globalAcceleration
          x.previous  <- x.current
          x.current   <- next

        static member New timeStep mass position velocity =
          let current   = position
          let previous  = position - (timeStep * (velocity : Vector))
          Particle (mass, current, previous)

      end
```

So the full particle data set is now two arrays `hotDatum, coldDatum`. For most algorithms we just process the hot data.

### Optimizing the verlet computation

When analyzing the jitted assembly code I found several inefficiencies like this:

```asm
; Loads Vector from array and store in local variable
00007ffb`8aa3436d f30f6f00        movdqu  xmm0,xmmword ptr [rax]
00007ffb`8aa34371 f30f7f442418    movdqu  xmmword ptr [rsp+18h],xmm0
00007ffb`8aa34377 4c8b4010        mov     r8,qword ptr [rax+10h]
00007ffb`8aa3437b 4c89442428      mov     qword ptr [rsp+28h],r8
; Loads same Vector from array and store in local variable
00007ffb`8aa34380 f30f6f00        movdqu  xmm0,xmmword ptr [rax]
00007ffb`8aa34384 f30f7f0424      movdqu  xmmword ptr [rsp],xmm0
00007ffb`8aa34389 4c8b4010        mov     r8,qword ptr [rax+10h]
00007ffb`8aa3438d 4c89442410      mov     qword ptr [rsp+10h],r8
; Loads first local variable into xmm0, xmm1, xmm2
00007ffb`8aa34392 4c8d442418      lea     r8,[rsp+18h]
00007ffb`8aa34397 f2410f1000      movsd   xmm0,mmword ptr [r8]
00007ffb`8aa3439c f2410f104808    movsd   xmm1,mmword ptr [r8+8]
00007ffb`8aa343a2 f2410f105010    movsd   xmm2,mmword ptr [r8+10h]
; Loads second local variable into xmm3, xmm4, xmm5, same content
00007ffb`8aa343a8 4c8d0424        lea     r8,[rsp]
00007ffb`8aa343ac f2410f1018      movsd   xmm3,mmword ptr [r8]
00007ffb`8aa343b1 f2410f106008    movsd   xmm4,mmword ptr [r8+8]
00007ffb`8aa343b7 f2410f106810    movsd   xmm5,mmword ptr [r8+10h]
00007ffb`8aa343bd f20f58c3        addsd   xmm0,xmm3
00007ffb`8aa343c1 f20f58cc        addsd   xmm1,xmm4
00007ffb`8aa343c5 f20f58d5        addsd   xmm2,xmm5
00007ffb`8aa343c9 4c8d442430      lea     r8,[rsp+30h]
; Store intermediate result in third local variable
00007ffb`8aa343ce f2410f1100      movsd   mmword ptr [r8],xmm0
00007ffb`8aa343d3 f2410f114808    movsd   mmword ptr [r8+8],xmm1
00007ffb`8aa343d9 f2410f115010    movsd   mmword ptr [r8+10h],xmm2
```

Overall there seems to be lots of roundtrips to and from invisible local variables that cost precious cycles as well as making the jitted code harder to understand.

I don't understand exactly why this is so but I suspect the jitter struggles to establish a holistic view that allows it to eliminate the intermediate results from each operation and therefore stores the results as invisible local variables to be sure. If the jitter was more advanced or had more CPU time available to it it should be able to eliminate the intermediate results.

To address this test case **Hot & Cold (No Algebra)** no longer rely on Vector algebra but instead uses a helper for the verlet integration:

```fsharp
  let inline verlet (current : Vector) (previous : Vector) (globalAcceleration : Vector) : Vector =
    let x = current.X + current.X - previous.X + globalAcceleration.X
    let y = current.Y + current.Y - previous.Y + globalAcceleration.Y
    let z = current.Z + current.Y - previous.Z + globalAcceleration.Z
    Vector (x, y, z)
```

This function triplicate the logic but now only rely on float algebra. Floats are well-known to the jitter and it might be able to eliminate local variables better in this case.

Let's have a look:

```asm
; Load ea for current into r8
00007ffb`8aa246c9 488d4108        lea     rax,[rcx+8]
00007ffb`8aa246cd 4c8bc0          mov     r8,rax
; Load x,y,z for current into xmm0, xmm1, xmm2
00007ffb`8aa246d0 f2410f1000      movsd   xmm0,mmword ptr [r8]
00007ffb`8aa246d5 f2410f104808    movsd   xmm1,mmword ptr [r8+8]
00007ffb`8aa246db f2410f105010    movsd   xmm2,mmword ptr [r8+10h]
; Load ea for previous into r8
00007ffb`8aa246e1 4c8d4120        lea     r8,[rcx+20h]
; Load x,y,z for current into xmm3, xmm4, xmm5
00007ffb`8aa246e5 f2410f1018      movsd   xmm3,mmword ptr [r8]
00007ffb`8aa246ea f2410f106008    movsd   xmm4,mmword ptr [r8+8]
00007ffb`8aa246f0 f2410f106810    movsd   xmm5,mmword ptr [r8+10h]
; current.X + current.X - previous.X + globalAcceleration.X
00007ffb`8aa246f6 f20f58c0        addsd   xmm0,xmm0
00007ffb`8aa246fa f20f5cc3        subsd   xmm0,xmm3
00007ffb`8aa246fe f20f5802        addsd   xmm0,mmword ptr [rdx]
; current.Y + current.Y - previous.Y + globalAcceleration.Y
00007ffb`8aa24702 0f28d9          movaps  xmm3,xmm1
00007ffb`8aa24705 f20f58d9        addsd   xmm3,xmm1
00007ffb`8aa24709 f20f5cdc        subsd   xmm3,xmm4
00007ffb`8aa2470d f20f585a08      addsd   xmm3,mmword ptr [rdx+8]
; current.Z + current.Y - previous.Z + globalAcceleration.Z
00007ffb`8aa24712 f20f58ca        addsd   xmm1,xmm2
00007ffb`8aa24716 f20f5ccd        subsd   xmm1,xmm5
00007ffb`8aa2471a f20f584a10      addsd   xmm1,mmword ptr [rdx+10h]
00007ffb`8aa2471f 488d1424        lea     rdx,[rsp]
; Vector (x, y, z)
00007ffb`8aa24723 f20f1102        movsd   mmword ptr [rdx],xmm0
00007ffb`8aa24727 f20f115a08      movsd   mmword ptr [rdx+8],xmm3
00007ffb`8aa2472c f20f114a10      movsd   mmword ptr [rdx+10h],xmm1
```

It looks much better as we don't see writes of intermediate results. In addition this is the full verlet computation, before we only had the initial steps of the computation in about the same number of lines.

The test case **Hot & Cold (No algebra)** should perform better thanks to this.

### Using `Structures of Arrays` over `Arrays of Structures`

Typically we create a class/struct and from that create an array that we iterate. This is somewhat comparable to `Row-Stores` in databases. Each row/array entry holds a full object. Many databases also supports `Column-Stores` as this is more efficient if you have lots of properties in an object but only accesses a few of them (like during report generation). We can apply the same approach to objects in memory which is called `Structures of Arrays (SOA)`. What we normally do is called `Arrays of Structures (AOS)`.

This will give a near optimal usage of cache and band-width but we get extra overhead because we will need to dereferenc several arrays for each computation.

For test case **Structures of Arrays** we implement it as this:

```fsharp
    type Selection =
      | A
      | B
    [<NoComparison>]
    [<NoEquality>]
    type Particles (timeStep, initParticles : InitParticle []) =
      class
        let count                    = initParticles.Length
        let masses      : float   [] = initParticles |> Array.map (fun ip -> ip.Mass)
        let positionsA  : Vector  [] = initParticles |> Array.map (fun ip -> ip.Position)
        let positionsB  : Vector  [] = initParticles |> Array.mapi (fun i ip ->
          let position = positionsA.[i]
          let velocity = ip.Velocity
          position - (timeStep * velocity)
          )
        let hotData     : HotData [] = Array.zeroCreate count
        let coldData    : ColdData[] = Array.zeroCreate count

        let mutable selection = Selection.A

        let rec loop globalAcceleration (a : Vector []) (b : Vector []) i =
          b.[i] <- verlet a.[i] b.[i] globalAcceleration
          if i > 0 then loop globalAcceleration a b (i - 1)

        member x.Verlet globalAcceleration =
          selection <-
            match selection with
            | Selection.A ->
              loop globalAcceleration positionsA positionsB (count - 1)
              Selection.B
            | Selection.B ->
              loop globalAcceleration positionsB positionsA (count - 1)
              Selection.A

        member x.Positions =
          match selection with
          | Selection.A -> positionsA
          | Selection.B -> positionsB

      end
```

So instead of having a `Particle` class we now have a class `Particles` that manages all particles. The idea is that mass, currentPosition and nextPosition properties are stored as arrays. A "`this`" pointer for a particle object is  the index into the arrays.

In addition, the example avoids copying floats by letting positionsA and positionsB switch the roles of currentPosition and nextPosition.

Another small optimization is to loop towards 0 as this saves a register which saves us loading the length from the stack if we run out of registers. The prefetcher fetches both before and after the address we accessed so it gives no negative impact on performance.

Checking the jitted code we see that the end of loop checking is not really optimal

```asm
00007ffb`8aa04bc3 85ff            test    edi,edi
00007ffb`8aa04bc5 7e07            jle     00007ffb`8aa04bce
00007ffb`8aa04bc7 ffcf            dec     edi
00007ffb`8aa04bc9 e977ffffff      jmp     00007ffb`8aa04b4a
```

It would be better if it looked like this instead:

```asm
00007ffb`8aa04bc7 ffcf            dec     edi
00007ffb`8aa04bc3 85ff            test    edi,edi
00007ffb`8aa04bc5 7e07            jge     00007ffb`8aa04b4a
```

For tight loops this can make a difference. I suspect this is due to how tail recursion is unpacked in F# and should be possible to improve upon.

### `Structures of Arrays` with .NET 4.6 SIMD

In .NET 4.6 we got SIMD enabled data structures like `System.Numerics.Vector3`. This should give a performance improvement. An issue with .NET 4.6 SIMD is that `System.Numerics.Vector3` only supports single precision floats. This divides the size of the test data by two so it's not a fair comparison. However, I thought it could be interesting to include anyway so I added test case **Structures of Arrays (SIMD)**.

Since the jitter recognizes the SIMD types we can use vector algebra and avoid lot hidden local variables in the jitted code:

```asm
; Checking out of bounds for current array
00007ffb`8aa14c8a 8b4308          mov     eax,dword ptr [rbx+8]
00007ffb`8aa14c8d 3bf0            cmp     esi,eax
00007ffb`8aa14c8f 7367            jae     00007ffb`8aa14cf8
00007ffb`8aa14c91 4863c6          movsxd  rax,esi
00007ffb`8aa14c94 486bc003        imul    rax,rax,3
; Loads current into xmm0 (xmm0 contains all three float32s as this is SIMD)
00007ffb`8aa14c98 488d548310      lea     rdx,[rbx+rax*4+10h]
00007ffb`8aa14c9d f30f104a08      movss   xmm1,dword ptr [rdx+8]
00007ffb`8aa14ca2 f20f1002        movsd   xmm0,mmword ptr [rdx]
00007ffb`8aa14ca6 0fc6c144        shufps  xmm0,xmm1,44h
; Checking out of bounds for previous array
00007ffb`8aa14caa 8b5508          mov     edx,dword ptr [rbp+8]
00007ffb`8aa14cad 3bf2            cmp     esi,edx
00007ffb`8aa14caf 7347            jae     00007ffb`8aa14cf8
; Loads previous into xmm1 (xmm1 contains all three float32s as this is SIMD)
00007ffb`8aa14cb1 488d448510      lea     rax,[rbp+rax*4+10h]
00007ffb`8aa14cb6 f30f105008      movss   xmm2,dword ptr [rax+8]
00007ffb`8aa14cbb f20f1008        movsd   xmm1,mmword ptr [rax]
00007ffb`8aa14cbf 0fc6ca44        shufps  xmm1,xmm2,44h
; current + current
00007ffb`8aa14cc3 0f58c0          addps   xmm0,xmm0
; - previous
00007ffb`8aa14cc6 0f5cc1          subps   xmm0,xmm1
; Loads globalAcceleration (would be better to have this preloaded into xmm2 but oh well)
00007ffb`8aa14cc9 f30f105708      movss   xmm2,dword ptr [rdi+8]
00007ffb`8aa14cce f20f100f        movsd   xmm1,mmword ptr [rdi]
00007ffb`8aa14cd2 0fc6ca44        shufps  xmm1,xmm2,44h
; + globalAcceleration
00007ffb`8aa14cd6 0f58c1          addps   xmm0,xmm1
; Stores result
00007ffb`8aa14cd9 f20f1100        movsd   mmword ptr [rax],xmm0
00007ffb`8aa14cdd 660f70c802      pshufd  xmm1,xmm0,2
00007ffb`8aa14ce2 f30f114808      movss   dword ptr [rax+8],xmm1
; End of loop?
00007ffb`8aa14ce7 85f6            test    esi,esi
00007ffb`8aa14ce9 7e04            jle     00007ffb`8aa14cef
; loop globalAcceleration a b (i - 1)
00007ffb`8aa14ceb ffce            dec     esi
00007ffb`8aa14ced eb96            jmp     00007ffb`8aa14c8a
```

Apart from not preloading `globalAcceleration` into a `xmm2` and the slight suboptimal end of loop check it looks pretty good and we expect **Structures of Arrays (SIMD)** to do pretty good.

## Measuring performance

These are the different test cases we like to measure:

0. **Class**                                  - Array of Particle classes in the order of creation
0. **Class (Shuffle)**                        - Array of Particle classes in random order
0. **Struct**                                 - Array of Particle structs in the order of creation
0. **Struct (Shuffle)**                       - Array of Particle structs in random order
0. **Hot & Cold**                             - Separated Hot & Cold into two arrays
0. **Hot & Cold (No Algebra)**                - Separated Hot & Cold into two arrays and avoids vector algebra
0. **Structures of Arrays**                   - Uses `SOA` over `AOS`
0. **Structures of Arrays (SIMD)**            - Uses `SOA` over `AOS` with .NET 4.6 SIMD Note; .NET SIMD only support single float precision so not exactly equivalent test.
0. **Structures of Arrays (SSE2)**            - Uses `SOA` over `AOS` with C++ SSE2 intrinsics
0. **Structures of Arrays (AVX)**             - Uses `SOA` over `AOS` with C++ AVX intrinsics

### Performance in Milliseconds, F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)

![Performance in Milliseconds, F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)](http://i.imgur.com/b5WMpcU.png)

### Performance in Milliseconds (Logarithmic), F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)

![Performance in Milliseconds (Logarithmic), F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)](http://i.imgur.com/uWh61Dw.png)

We clearly see that **Class (Shuffle)** degrades a lot compared to the alternatives. This is because when the data no longer fits into the L3 cache **and** we access the data randomly we get no benefit of the prefetcher and the CPU has to wait a long time for data. We also see that the **Class (Shuffle)** starts degrading in several steps. This is because there is prefetching between L1, L2 and L3 as well.

In no other cases shuffling seems to effect performance negatively as expected.

**Class**, **Struct** and **Struct (Shuffle)** degrades somewhat when we run out of L3 cache but because of prefetching it never gets as bad as **Class (Shuffle)**.

**Hot & Cold** (both variants) don't seem to degrade even though we run out L3 cache. This is because the prefetcher manages to stay ahead of the CPU.

**Hot & Cold (No Algebra)** does better because we managed to eliminate writes of intermediate results by refactoring the code.

**Structures of Arrays** (both variants) does the best. As **Hot & Cold** doesn't degrade due to data starvation it sholdn't be because of more efficient use band-width. Instead, it's mainly because the way the code is written we avoid a method call for each particle. When we are calling a method we first marshalls the input arguments and then the method unmarshalls them. As the verlet computation is quite cheap the method call itself is taking some time.

**Structures of Arrays (SIMD)** is the fastest alternative which we were expecting.

In order to easier estimate the order on how the performance depend on the data size I provide a chart where x-axis and y-axis is logarithmic

### **Update 2017-01-05**: Added C++ AVX and SSE results.

Added results for **Structures of Arrays (SSE2)** and **Structures of Arrays (AVX)**.

The assembler looks pretty good for (AVX)

```asm
; Note: AVX processes 4 doubles per operation
; Load current x 4
00007FF6E4ED14E0  vmovupd     ymm0,ymmword ptr [r10+rdx]
; Load previous x 4
00007FF6E4ED14E6  vmovupd     ymm1,ymmword ptr [rdx]
; Save rdx for when storing result
00007FF6E4ED14EA  mov         rax,rdx
; Increment pointer
00007FF6E4ED14ED  add         rdx,20h
; current + current x 4
00007FF6E4ED14F1  vaddpd      ymm0,ymm0,ymm0
; - previous x 4
00007FF6E4ED14F5  vsubpd      ymm1,ymm0,ymm1
; + globalAccelerator x 4
00007FF6E4ED14F9  vaddpd      ymm2,ymm1,ymm4
; Store result x 4
00007FF6E4ED14FD  vmovapd     ymmword ptr [rax],ymm2
; Decrement counter
00007FF6E4ED1501  sub         r8,1
; If > 0 then loop
00007FF6E4ED1505  jne         `anonymous namespace'::particles<`anonymous namespace'::avx>::verlet_axis+80h (07FF6E4ED14E0h)
```

The results are interesting. The AVX code is as expected doing really well but then surprisingly drops off sharply at 586 particles. My assumption is that we run out of L1 cache at this point but because we are hitting the band-width limit of L2 the prefetcher can't keep up. L1 peak rate seems to be around 74 GiB/second, when we hit L2 the rate falls to around 43 GiB/second)

The reason AVX and SSE2 falls behind **Structures of Arrays (SIMD)** should be that the latter uses `float32` over `float` which means the band-width requirement is about half.

### Performance in Milliseconds (Annotated), F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)

![Performance in Milliseconds (Annotated), F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)](http://i.imgur.com/wluJ7GS.png)

For **Class (Shuffle)** one sees three plateaus in the test data that are delimited by a linear growth of performance (as the axes are both logarithmic).

I have marked the three plateaus with `A`, `B` and `C`.

An explanation for the three plateaus could be that the `A` plateau is when the data fits into L1 cache. For plateau `B` the data still fit into L2 but because L2 is slower the performance degrades somewhat. `C` plateau then matches L3 cache.

As nice explanation as this is it doesn't match the input data.

Consider plateau `B`. It starts at 210 particles and end at 1485 particles. Since the size of a particle reported by .NET is 216 bytes (also matches manual estimate) that would mean the `A` runs out at 44KiB and `B` runs out at 313KiB but that doesn't match the numbers for my machine. In addition, plateau `C` ends at 1830KiB.

The plateaus are interesting but I don't know how to correctly interpret them at this point.

Finally, it seems that the performance for **Class (Shuffle)** asymptotically goes towards 1200 ms. As it starts at 200ms it means that the memory latency overhead is 1 second. We are processing 10,000,000 particles that means that the latency per particle is 100 ns which is quite close to the 120 ns reported by Intel® Memory Latency Checker tool.

### **Update 2017-01-04**: The plateaus now makes more sense.

A former colleague of mine, Henning, pointed out that the Core I5 has 64KiB of L1 cache per core, the total for all 4 cores is 256KiB. In addition, only 32KiB is data cache the other 32KiB is instruction cache. The L2 cache is also just 256KiB per core but 1MiB in total. The L3 cache is 6MiB shared for all 4 cores.

So plateau `A` ends at 210 particles which is then 44KiB, this number is close to 32KiB L1 data cache available to the core. Looking more closely at the charts it seems plateau `B` ends at at 1123 particles which is 236KiB which is close to the 256KiB L2 cache available to the core.

So the numbers and the input data matches decently for L1 and L2 cache now.

## Conclusions

To be honest most developers don't need top-notch CPU performance, most are happy with decent CPU performance. However, I know that there are a few out there that needs to process a lot of data in a short amount of time.

For those guys it's very important to minimize the size of the data (in order to fit as much in cache as possible) as well as making sure to access the data sequentially (to make efficient use of the prefetcher).

A managed language environment like .NET and Java doesn't allow direct control of the memory layout of objects but since the GC tries to maintain the order the objects were created as long as we allocate them in one go (to avoid hole in the allocations) and access them in that order we make good use of the prefetcher.

In addition, .NET has `struct` types that allows us to create arrays of `struct` which guarantees that as long as we iterate the array in order we access the memory sequentially.

A common pattern to make better use of the memory bandwidth and cache is to split the data into hot and cold data. However, this requires intimate knowledge of the data access patterns or testing to get right.

That's why I think `Structures of Arrays (SOA)` is interesting as it will allow optimum usage of the cache and bandwidth but with the drawback the we need to dereference several arrays during the computation. When we run out of registers that means loading the pointers from the stack which itself is costly. Testing again is important to get it right.

The blog post was intended to just be about data sizes and how it effects performance but when I started looking at the jitted code I found some efficiency problems I just **had** to address. I hope it's ok.

I hope this was interesting to you.

Mårten
