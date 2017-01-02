# On the topic of data locality

It is well-known that a harddisk has a pretty long latency from that we request the data to that we get the data. Usually we measure the harddisk latency in milliseconds which is an eternity for a CPU. The bandwidth of a harddisk is pretty good though as SSD:s today can reach 1 GiB/second.

What is less known is that RAM has the same characteristics, really bad latency but good bandwidth.

The RAM latency for my machine under semi-high load is ~120 ns (measured using the wonderful program [Intel® Memory Latency Checker](https://software.intel.com/en-us/articles/intelr-memory-latency-checker)). This means that the CPU on my machine has to wait for ~400 cycles for data from RAM.

Therefore over the years CPU:s has accumulated more and more cache as well as advanced techniques to schedule reads earlier and delaying writes to avoid starving for data. So much in fact that only ~1% of the CPU die is used for computation and the rest is used to fix the problem of slow RAM.

In order to write high-performant code that process a lot of data we have to read data sequentially from RAM (just as we would have to from harddisk) as this allows the CPU to efficiently prefetch data for us.

## What is a lot of data?

On my machine the L1 cache is 256KiB which means if the data fits into 256KiB we don't have to think. L2 is 1MiB and L3 is 6MiB. So if my data is more than 6MiB I probably should start thinking on accessing the data sequentially if I care about performance.

One of the fundamental principles of .NET and Java is that memory is managed. .NET prevents developers from unintentionally corrupting data and has a GC that can defragment the heap for us. .NET also hides the location of the memory for us as it's not important for the correctness of a program. The problem is that the location is very important for us when it comes to writing performant code that process a lot of data.

## How do we make sure we read the data sequentially?

One of the simplest ways to access data sequentially is to use arrays. Arrays are a continous block of memory, if we traverse it from `0..last` we access the array data sequentially.

A bit of a snag is in .NET the actual data in an array is often a reference to an object which may or may not lay sequential in memory. The GC tries to preserve the order of the objects in the way they were created but it's not certain that correlates to how you will access the data.

.NET allows us to create arrays of `struct`. A `struct` in .NET is a value object which means that when we iterate an array of `struct` we are indeed accessing the data sequentially.

## Measuring the cost of data access

In order to measure different data access schemes I have created a few test cases that uses various patterns to access data.

The basic premise that we have a large set of particles that contain:

1. Mass
2. Current position
3. Previous position
4. Hot Data (data that would be accessed often)
5. Cold Data (data that would be accessed seldom)

We will perform something called verlet integration over all particles which is a common in game physics. It's a very simple computation

```fsharp
  let newPos  =  currentPos + currentPos - previousPos + globalAcceleration
  previousPos <- currentPos
  currentPos  <- newPos
```

This computation only access current and previous position and doesn't look at neither the hot data or the cold data which is common for these kind of algorithms.

We obviously like the verlet integration to be as fast as possible.

With the mass, current and previous position, hot data and cold data a particle takes approximately 200 bytes. We will have up to 1,000,0000 particles which means the particles will take approximately 200,000,0000 bytes which is way more than my 6MiB of L3 cache. Therefore we expect that the way we access data will have an impact.

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

1. In the first array all objects are stored in the order they are created which mean the particles will be sequential in memory when we iterate the array
2. In the second array we shuffle the order of the objects in order to simulate a situation where we for some reason don't access the objects in the order they were created. *Note: if you store particles in a dictionary and iterate it the order is effectively random*

For small sets of data we expect no difference in performance when iterating the arrays but what about larger sets?

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

In the test case **Hot & Cold** we have split the data in two parts, hot and cold. In our fictious program we have identified hot data (data that is accessed often) and grouped together, cold data is put into a different group. For this particular algorithm we only need to access the hot data.

This is common approach make sure we don't waste memory bandwidth on loading cold data which we don't access during verlet integration. We still have some overhead as there are hot data the verlet interation doesn't accessed but that may be accessed later during constraint relaxation.

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

I don't understand exactly why this is so but I suspect the jitter guarantees some invariants this way, if the jitter was more advanced or had more CPU time available to it it should be able to eliminate the local variables.

To address this so in the test case **Hot & Cold (No Algebra)** I know longer rely on Vector algebra but instead implement a helper function like this for the verlet integration:

```fsharp
  let inline verlet (current : Vector) (previous : Vector) (globalAcceleration : Vector) : Vector =
    let x = current.X + current.X - previous.X + globalAcceleration.X
    let y = current.Y + current.Y - previous.Y + globalAcceleration.Y
    let z = current.Z + current.Y - previous.Z + globalAcceleration.Z
    Vector (x, y, z)
```

This function triplicate the logic but now it only rely on float algebra. Floats are well-known to the jitter and it might be able to eliminate local variables better in this case.

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

It looks much better and test case **Hot & Cold (No algebra)** should perform better thanks to this.

### Using `Structures of Arrays` over `Arrays of Structures`

Typically we create a class/struct and from that create an array that we iterate. This is somewhat comparable to `Row-Stores` in databases. Each row/array entry holds a full object. Many databases also supports `Column-Stores` as this is more efficient if you have lots of properties in an object but only accesses a few of them (like during report generation). We can apply the same approach to objects in memory which is called `Structures of Arrays (SOA)`. What we normally do is called `Arrays of Structures (AOS)`.

We implement it as this:

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

The idea is that mass, currentPosition and nextPosition properties are stored as arrays. The `this` pointer for a particle object is then the index in the arrays.

In addition the example avoids copying floats by letting positionsA and positionsB switch the roles of currentPosition and nextPosition.

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

In .NET 4.6 we got SIMD enabled data structures like `System.Numerics.Vector3`. This should give a performance improvement. An issue with .NET 4.6 SIMD is that `System.Numerics.Vector3` only supports single precision floats. This divides the size of the test data by two so it's not a fair comparison. However, I thought it could be interesting to include anyway:

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

## Performance in Milliseconds, F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)

![Performance in Milliseconds, F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)](http://i.imgur.com/yMhzGG1.png)

We clearly see that **Class (Shuffle)** degrades a lot compared to the alternatives. This is because that when the data no longer fits into the L3 cache **and** we access the data randomly we starve the CPU for data.

In no other cases shuffling seems to effect performance negatively as expected.

**Class**, **Struct** and **Struct (Shuffle)** degrades somewhat when we run out of L3 cache but because of prefetching it never gets as bad as **Class (Shuffle)**.

**Hot & Cold** (both variants) does a lot better because even though it also eventually runs out L3 cache because we only fetch hot data we make it easier for the prefetcher to stay ahead of us.

**Structures of Arrays** (both variants) does the best for all sizes as it seems updating the arrays is faster than the updating fields. This is a surprising result as I was expecting `SOA` to do worse for small data sets but then outpace at bigger data sets. I will need to look at the jitted assembly code to understand this.

In order to easier estimate the order on how the performance depend on the data size I provide a chart where x-axis and y-axis is logarithmic

![Performance in Milliseconds (Logarithmic), F# 4, .NET 4.6.2, X64 (256KiB L1, 1MiB L2, 6MiB L3)](http://i.imgur.com/rXp40Kw.png)

For **Class (Shuffle)** one sees three plateaus in the test data that are delimited by a linear growth of performance (as the axes are both logarithmic).

I have mark the three plateaus with `L1`, `L2` and `L3` with the assumption that for **Class (Shuffle)** this is when the data is able to fit inside the different layers of cache.

As nice explaination as this is it doesn't match the input data.

Consider plateau `L2`. It starts at 256 particles and end at 1389 particles. Since the size of a particle reported by .NET is 216 bytes (also matches manual estimate) that would mean the L1 runs out at 54KiB and L2 runs out at 292KiB but that doesn't match the numbers for my machine. In addition, plateau `L3` ends at 1920KiB.

The plateaus are interesting but currently the numbers don't add up for me.

## Conclusions

To be honest most developers don't need top-notch performance, most are happy with decent performance. However, I know that there are a few out there that needs to process a lot of data in a short amount of time.

For those guys it's very important to minimize the size of the data (in order to fit as much in cache as possible) as well as making sure to access the data sequentially (to make efficient use of the prefetcher).

A managed language environment like .NET and Java doesn't allow direct control of the memory layout of objects but since the GC tries to maintain the order the objects were created as long as we allocate them in one go (to avoid hole in the allocations) and access them in that order we make good use of the prefetcher.

In addition, .NET has `struct` types that allows us to create arrays of `struct` which guarantees that as long as we iterate over the array in order we access the memory sequentially.

A common pattern to make better use of the memory bandwidth and cache is to split the data into hot and cold data. However, this requires intimate knowledge of the data access patterns or testing to get right.

That's why I think `Structures of Arrays (SOA)` is interesting as that is taking will allow optimum usage of the cache and bandwidth but with the drawback the we need to lookup data in several arrays to the computation. When we run out of registers that means loading the pointers from the stack which itself is costly. Testing again is important to get it right.

I was suprised about the excellent performance of `Structures of Arrays (SOA)` version. I will need to revisit the jitted code in order to understand why `SOA` performed better than the other alternatives even for smaller data sets. One possibility is just buggy code but I compared the results of the different algorithms and they did come up equal.

I hope this was interesting to you.

Mårten
