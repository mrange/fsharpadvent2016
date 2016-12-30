# On the cost of being lazy

*[Full source code can be found here](https://github.com/mrange/fsharpadvent2016/tree/master/src/perflazy)*

Previously I have looked at performance of different [persistent hash maps](https://gist.github.com/mrange/d6e7415113ebfa52ccb660f4ce534dd4) and [data pipelines](https://gist.github.com/mrange/09ba6d7541468de49426eeb81a8b660c) in .NET. Now I wonder:

*What is the cost of being lazy?*

In F# we can define a lazy value like this:

```fsharp
let x = lazy expensiveCalculation ()  // Delay the expensive computation
let y = x.Value                       // Forces the computation and returns the cached result
let z = x.Value                       // Returns the cached result
```

What is the CPU cost of this delayed computation and how does `lazy` compare with alternatives?

To test it I have created a simple algorithm that allows me to control the ratio of how often the lazy value will be be recreated so that we can see how the relative cost of being lazy depends on the ratio. The delayed computation is very cheap (returns an integer) as we like to measure the cost of being lazy.

```fsharp
let inline delay i              = lazy i
let inline value (l : Lazy<_>)  = l.Value


let createTestCases count ratio =
  let rec simpleLoop l r s i =
    if i < count then
      let r = r + ratio
      if r >= 1. then
        let r = r - 1.
        let l = delay i // Reset the lazy value to force recomputation
        simpleLoop l r (s + value l) (i + 1)
      else
        simpleLoop l r (s + value l) (i + 1) // Use the cached value
    else
      s

  let simple () =
    simpleLoop (delay 0) 0. 0 0

  simple
```

I vary the ratio from `0%` (only uses the cached value) to `100%` (performs computation every time) in step of `20%`.

These are the alternatives I selected:

0. **no lazy**                         - The control, the computation is done every time. As the computation is extremely cheap we expect this to be the fastest and we will just the control to compare the cost of being lazy.
0. **lazy**                            - Uses `F#` lazy
0. **Lazy (Execution & Publication)**  - `F#` internally uses .NET `Lazy<'T>` with Execution & Publication protection so we expect the performance to be identical.
0. **Lazy (Publication)**              - Same as above but here we only uses publication protection. That means that the computation might be carried out by several threads but only one of the results are cached. There subtleties that I haven't dug into yet on whether all threads will see the same instance or not?
0. **Lazy (None)**                     - No thread safety, if multiple threads are dereferencing the lazy value the behavior is undefined.
0. **Flag (Trivial)**                  - A lazy implementation using a simple unprotected flag to decide whether a value is computed or not. Doesn't cache exceptions.
0. **Flag (Compact)**                  - A lazy implementation letting the value act both value and flag. Doesn't cache exceptions.
0. **Flag (Exception aware)**           - A lazy implementation using a simple unprotected flag to decide whether a value is computed or not. Does cache exceptions.
0. **Flag (Protected)**                - A lazy implementation that uses a simple publication protection scheme. Unfortunately so simple that different threads might see different instances. Doesn't cache exceptions.
0. **Flag (Full protection)**          - A lazy implementation that uses `Monitor` to achieve Execution & Publication protection. Doesn't cache exceptions.
0. **Flag (Full protection w. DC)**    - A lazy implementation that uses `Monitor` to achieve Execution & Publication protection and Double Check pattern to reduce cost of reading a cached value. Doesn't cache exceptions.

## Performance in Milliseconds - F# 4, .NET 4.6.2, x64

![Performance in Milliseconds - F# 4, .NET 4.6.2, x64](http://i.imgur.com/vrzBoNm.png)

As expected **no lazy** is the fastest overall (because the computation is very cheap).

At `0%` all lazy implementations except **Flag (Compact)** and **Flag (Full protection)** has similar performance. **Flag (Compact)** does a type check each time it is dereferenced, this is the cost we see. **Flag (Full protection)** does a `Monitor.Enter`/`Monitor.Leave` when it's dereferenced which is costly compared to the other alternatives.

For all lazy implementations except **Flag (Full protection)** the overhead seems to be roughly linear to the ratio.

At `100%` we see that **lazy** incurs a 50x performance overhead. It is also somewhat surprising that **lazy** does worse than **Lazy (Execution & Publication)** as `F#` uses the same implementation but it turns out that `FSharp.Core` allocates an extra function object when we use `lazy`.

Because of the forgiving memory model of `i86` it turns out that the cost of **Flag (Trivial)** and **Flag (Protected)** is almost identical. This might not be true on `ARM` or `PowerPC`.

In all these cases we have no contention of the locks so the performance will look different in a concurrent environment with heavy contention. What is the cost of locks under contention? That is a topic for another blog post.

## Collection Count in Milliseconds - F# 4, .NET 4.6.2, x64

![Collection Count in Milliseconds - F# 4, .NET 4.6.2, x64](http://i.imgur.com/XcTkKv3.png)

Collection count gives an indication on the number of objects allocated by the lazy implementations. Lower is better.

For all lazy implementations the overhead seems to be roughly linear to the ratio.

All flag implementations except **Flag Compact** incurs the same overhead where **Flag Compact** ironically has a higher overhead. This is because **Flag Compact** needs to box the integer value.

## Reflections

Is it a problem that for `lazy` at `100%` the overhead is large? Isn't it so that in most realistic cases the ratio will be closer to `0%` than `100%`. In at least one case the opposite is true, consider [`Seq.upto`](https://github.com/Microsoft/visualfsharp/blob/master/src/fsharp/FSharp.Core/seq.fs#L274). It turns out `Seq.upto` uses `Lazy<_>` to cache the current value (or exception). Normally the current value is accessed just once and then the next value is computed. That means in this case the ratio is closer to `100%` and then incurs a large overhead if the computation is cheap. During my comparison of data pipelines in .NET this turned out to be the source of most of the overhead of `Seq` over other pipelines.

Personally, I am sceptical of mutually exclusive regions of code (like `Monitor.Enter`/`Monitor.Exit`). One of my reasons for this is the dreaded priority inversion. Now on `Windows` and non real-time `Linux` this isn't much of a problem in general but when working with real-time system priority version is real problem. In addition, the real-time distributions of `Linux` I had the pleasure of working with had severe problems in the futex implementation which meant that a futex could lock-out a different process (!) if we are unlucky. So by avoiding mutually exclusive regions we avoid these problems.

On the other hand, implementing thread-safe code without mutually exclusive regions is really hard as it requires whole different understanding of how the compiler, jitter CPU, cache and memory system rewrites your code in order to speed it up. Basically, reads are scheduled earlier and writes are delayed. Reads & writes that are deemed unnecessary may be eliminated. These rewrites works brilliantly in a **non-concurrent** environment. In a **concurrent** the rewrites makes it impossible for you to look at the code and deduce what happens (because this is not the program that is being executed). Because of this the simple [double-check locking](https://en.wikipedia.org/wiki/Double-checked_locking) is actually quite hard to implement. Mutually exclusive regions restores sanity but as mentioned above has their own problem.

Finally, know that being lazy has costs. If the computation is cheap it might be better to do the computation repeatedly instead of caching the result.

Hope this was interesting to you,

Mårten
