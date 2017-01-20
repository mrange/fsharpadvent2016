# On the topic of 2D array performance in .NET

*[Full source code can be found here](https://github.com/mrange/fsharpadvent2016/tree/master/src/perf2d)*

It popped up a discussion in FSSF Slack over 2D arrays and performance. I thought it could be good to share some performance numbers for different alternatives in .NET.

For this I am computing the matrix product:

```fsharp
let naiveMultiply () =
  for row = 0 to (a1 - 1) do
    for col = 0 to (b2 - 1) do
      let mutable sum = 0.
      for i = 0 to (a2 - 1) do
        sum <- sum + a.[row, i]*b.[i, col]
      r.[row, col] <- sum

  sum r
```

I vary the size of the matrix input but make sure that 1,000,000,000 cells is processed in each measurement to make the numbers comparable.

I use two variants of matrix multiplication, a **naive** version and a **better** version. The **better** version has better cache locality when the matrix size grows.

I use 3 variants of matrix representation:

0. **float[,]**   - The obvious representation of a matrix is a 2D array
0. **float[][]**  - Another obvious representation of a matrix is a so called a jagged array or an array of array
0. **float[]**    - Slightly less obvious is to use a 1D array with enough space to hold all matrix values and then we compute the index into the array like this `row*row_width + column`

Without further ado here are the results.

## Performance in Milliseconds - F# 4, .NET 4.6.1, x64

![Performance in Milliseconds - F# 4, .NET 4.6.2, x64](http://i.imgur.com/Cz2rRG6.png)

*Note that the scale is logarithmic*

From the results we see that **float[,]** performs the worst in general. This is because that the jitted code does a lot of work when dereferencing a 2D array, like for instance taking into account the base index for each axis (which is 0 in my case) . For 1D arrays the jitted code is a lot more efficient.

In most cases the **naive** version does better on smaller cases. This is because the code dereferences the result array less often.

For larger matrix size the performance of the **naive** version degrades a lot because that when `i` change in each iteration `b.[i, col]` will cause cache misses.

The **better** version the loops are changed so that the inner loop loops over `col` meaning `a.[row, i]` is constant and `b.[i, col]` has better cache locality.

**float[][]** **naive** version is doing the worst for large matrix sizes but **float[][]** **better** version does best overall.

I hope you found this interesting.

Mårten

# Appendix

## So why is **float[,]** doing worse than **float[][]**?

The answer to this question is in the jitted code. Let's start by looking at the inner loop for the **float[][]**.

```fsharp
for col = 0 to (b2 - 1) do
  rr.[col] <- rr.[col] + av*br.[col]
```

The annotated jitted code looks like this:

```asm
; Load rr.Length
00007ffa`6e017202 418b4c2408      mov     ecx,dword ptr [r12+8]
; Out of bounds?
00007ffa`6e017207 3bc1            cmp     eax,ecx
00007ffa`6e017209 7367            jae     00007ffa`6e017272
00007ffa`6e01720b 4863c8          movsxd  rcx,eax
; Load rr.col
00007ffa`6e01720e f2410f1044cc10  movsd   xmm0,mmword ptr [r12+rcx*8+10h]
; Load br.Length
00007ffa`6e017215 8b4d08          mov     ecx,dword ptr [rbp+8]
; Out of bounds?
00007ffa`6e017218 3bc1            cmp     eax,ecx
00007ffa`6e01721a 7356            jae     00007ffa`6e017272
00007ffa`6e01721c 4863c8          movsxd  rcx,eax
; Load br.[col]
00007ffa`6e01721f f20f104ccd10    movsd   xmm1,mmword ptr [rbp+rcx*8+10h]
; av*br.[col]
00007ffa`6e017225 f20f59ce        mulsd   xmm1,xmm6
; rr.[col] + av*br.[col]
00007ffa`6e017229 f20f58c1        addsd   xmm0,xmm1
00007ffa`6e01722d 4863c8          movsxd  rcx,eax
; Store rr.[col]
00007ffa`6e017230 f2410f1144cc10  movsd   mmword ptr [r12+rcx*8+10h],xmm0
; col <- col + 1
00007ffa`6e017237 ffc0            inc     eax
; Done or loop?
00007ffa`6e017239 3bc2            cmp     eax,edx
00007ffa`6e01723b 75c5            jne     00007ffa`6e017202
```

Apart from unnecessary checking of out of bounds conditions it looks pretty good. It could be better if the jitter was able to figure out that this code could be parallalized using SSE2/AVX but it isn't bad.

Let's look at the inner loop for **float[,]**:

```fsharp
for col = 0 to (b2 - 1) do
  r.[row, col] <- r.[row, col] + av*b.[i, col]
```

The annotated jitted code looks like this:

```asm
; Load r pointer
00007ffa`6e015178 488b4e18        mov     rcx,qword ptr [rsi+18h]
00007ffa`6e01517c 448bc7          mov     r8d,edi
; Subtract r.base1 from row
00007ffa`6e01517f 442b4118        sub     r8d,dword ptr [rcx+18h]
; row Out of bounds?
00007ffa`6e015183 443b4110        cmp     r8d,dword ptr [rcx+10h]
00007ffa`6e015187 0f83d3000000    jae     00007ffa`6e015260
00007ffa`6e01518d 448bc8          mov     r9d,eax
; Subtract r.base2 from col
00007ffa`6e015190 442b491c        sub     r9d,dword ptr [rcx+1Ch]
; col Out of bounds?
00007ffa`6e015194 443b4914        cmp     r9d,dword ptr [rcx+14h]
00007ffa`6e015198 0f83c2000000    jae     00007ffa`6e015260
; Load r.length2
00007ffa`6e01519e 448b5114        mov     r10d,dword ptr [rcx+14h]
; Multiply r.length2 with (row - r.base1)
00007ffa`6e0151a2 4d0fafd0        imul    r10,r8
00007ffa`6e0151a6 4d8bc1          mov     r8,r9
; Add with (col - r.base2), this forms the r store offset
; Now we do the same thing again to compute the load offset (it's the sama)E
00007ffa`6e0151a9 4d03c2          add     r8,r10
00007ffa`6e0151ac 448bcf          mov     r9d,edi
; Subtract r.base1 from row
00007ffa`6e0151af 442b4918        sub     r9d,dword ptr [rcx+18h]
; row Out of bounds?
00007ffa`6e0151b3 443b4910        cmp     r9d,dword ptr [rcx+10h]
00007ffa`6e0151b7 0f83a3000000    jae     00007ffa`6e015260
00007ffa`6e0151bd 448bd0          mov     r10d,eax
; Subtract r.base2 from col
00007ffa`6e0151c0 442b511c        sub     r10d,dword ptr [rcx+1Ch]
; col Out of bounds?
00007ffa`6e0151c4 443b5114        cmp     r10d,dword ptr [rcx+14h]
00007ffa`6e0151c8 0f8392000000    jae     00007ffa`6e015260
; Load r.length2
00007ffa`6e0151ce 448b5914        mov     r11d,dword ptr [rcx+14h]
; Multiply r.length2 with (row - base1)
00007ffa`6e0151d2 4d0fafd9        imul    r11,r9
00007ffa`6e0151d6 4d8bca          mov     r9,r10
; Add with (col - r.base2), this forms the r load offset
00007ffa`6e0151d9 4d03cb          add     r9,r11
; Load r.[row, col]
00007ffa`6e0151dc f2420f1044c920  movsd   xmm0,mmword ptr [rcx+r9*8+20h]
; Load b pointer
00007ffa`6e0151e3 4c8b4e10        mov     r9,qword ptr [rsi+10h]
00007ffa`6e0151e7 448bd5          mov     r10d,ebp
; Subtract r.base1 from i
00007ffa`6e0151ea 452b5118        sub     r10d,dword ptr [r9+18h]
; i Out of bounds?
00007ffa`6e0151ee 453b5110        cmp     r10d,dword ptr [r9+10h]
00007ffa`6e0151f2 736c            jae     00007ffa`6e015260
00007ffa`6e0151f4 448bd8          mov     r11d,eax
; Subtract r.base2 from col
00007ffa`6e0151f7 452b591c        sub     r11d,dword ptr [r9+1Ch]
; col Out of bounds?
00007ffa`6e0151fb 453b5914        cmp     r11d,dword ptr [r9+14h]
00007ffa`6e0151ff 735f            jae     00007ffa`6e015260
; Load b.length2
00007ffa`6e015201 458b7914        mov     r15d,dword ptr [r9+14h]
; Multiply b.length2 with (i - b.base1)
00007ffa`6e015205 4d0faffa        imul    r15,r10
00007ffa`6e015209 4d8bd3          mov     r10,r11
; Add with (col - b.base2), this forms the r load offset
00007ffa`6e01520c 4d03d7          add     r10,r15
; Load b.[i, col]
00007ffa`6e01520f f2430f104cd120  movsd   xmm1,mmword ptr [r9+r10*8+20h]
; av*b.[i, col]
00007ffa`6e015216 f20f59ce        mulsd   xmm1,xmm6
; r.[row, col] + av*b.[i, col]
00007ffa`6e01521a f20f58c1        addsd   xmm0,xmm1
; Store r.[row, col]
00007ffa`6e01521e f2420f1144c120  movsd   mmword ptr [rcx+r8*8+20h],xmm0
; col <- col + 1
00007ffa`6e015225 ffc0            inc     eax
; Done or loop?
00007ffa`6e015227 3bc2            cmp     eax,edx
00007ffa`6e015229 0f8549ffffff    jne     00007ffa`6e015178
```

We see several inefficiencies

0. Loading of pointers from stack
0. Even though `row` and `i` is kept constant the jitted code is constantly checking for out of bounds
0. Even though `row` and `i` is kept constant the jitted code is computing the offset
0. Load and Store offset for `r` is identical but is computed separately anyway

Compared to the rather efficient code for **float[][]** it's actually a bit surprising **float[,]** doesn't fall behind more.

