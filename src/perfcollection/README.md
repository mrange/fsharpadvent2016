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
