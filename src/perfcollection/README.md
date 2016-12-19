# Performance comparison of different data pipelines in .NET

As F# developers we all know about **Seq** that allows us to transform data using pipelines:

```fsharp
let result =
  Seq.init count id
  |> Seq.map    (fun n -> int64 n * 5L)
  |> Seq.filter (fun n -> n % 7L <> 0L)
  |> Seq.map    (fun n -> n / 11L)
  |> Seq.sum
```

There are many data pipelines to chose from.

0. **Imperative** - If we weren't F# developers we would use imperative patterns to implement the data pipeline
1. **Seq** - Good old **Seq** from `FSharp.Core`
2. **Linq** - **Linq** + some F# idiomatic wrappers is totally ok
3. **Nessos.Streams** - The gurus at *Nessos* created a data pipeline that supports push, pull, parallelism, cancelability and so on. It's also supposed to be really fast
5. **SeqComposer** - There's a PR in F# repository that intends to improve performance of **Seq**.
5. **PullStream** - I implemented a simple pull (like **Seq**) data pipeline to compare against
6. **PushStream** - I implemented a simple push (like supported by **Nessos.Streams**) data pipeline to compare against
7. **PushPipe** - A variant of **PushStream** intended to reduce the overhead of creating data pipelines.
8. **Nessos.LinqOptimizer** - While **Nessos.Streams** is cool, compared to **Nessos.LinqOptimizer** it pales as the later optimizes the data pipeline when constructed to improve performance.

I asked myself; *which data pipeline has the lowest overhead?*

In order to test this I created a simple pipeline that I implemented for all variants above. I was inspired by a data pipeline they use to test the **SeqComposer** PR.

```fsharp
let simple () =
  range count
  |> map    (fun n -> int64 n * 5L)
  |> filter (fun n -> n % 7L <> 0L)
  |> map    (fun n -> n / 11L)
  |> sum
```

1. To measure the overhead of the data pipeline itself we keep the work in the lambda expressions small.
2. As the source is a simple range and the result is a sum the memory consumption is a result of the data pipeline.
3. In addition, I vary the collection size from 1 to 10,000,000 but keep the overall useful work the same so that we can compare the times.

So without further ado:

## Performance in Milliseconds - F# 4, .NET 4.6.2, x64

![Performance in Milliseconds - F# 4, .NET 4.6.2, x64](http://i.imgur.com/KJgNejl.png)

## Collection Count - F# 4, .NET 4.6.2, x64

![Collection Count - F# 4, .NET 4.6.2, x64](http://i.imgur.com/qcH1d73.png)

## Interpreting the results

*Note that the axis uses logarithmic scale*

The imperative code is the fastest in general. The reason for this is that it is implemented as an efficient for loop. Other pipelines creates intermediate objects that support constructing the pipeline, this creates memory pressure. In order to pass values down the pipeline virtual dispatch is used which is hard for the JIT:er to inline.

The push pipelines (like **Nessos.Streams**) seems to perform better in general than pull pipeline. One of the reasons for that is that in a pull pipeline for each "pipe" there has to be a conditional check if the end is reached. For push pipelines the conditionals are largely eliminated.

The reason that Collection Count is higher for small collections is that if we run with collection size =1 then we create 10,000,000 pipelines. If there's memory overhead from creating a pipeline this shows up for smaller collection sizes. This inspired me to create the **PushPipe** that allows reusing the pipeline over several runs. I think my design can be improved a lot but we see that **PushPipe** is missing from the *Collection Count - F# 4, .NET 4.6.2, x64* because it doesn't have to recreate the pipeline. This also makes the CPU overhead lower for smaller collection sizes.

**Nessos.LinqOptimizer** is both among the worst and among the best. The reason is that there's a huge overhead of creating the data pipeline because **Nessos.LinqOptimizer** analyzes the expression trees, optimizes them, compiles them into ILCode which is then JIT:ed. This gives almost "imperative" performance for large collections but poor performance for small collections. I think **Nessos.LinqOptimizer** could benefit from being able to cache and reuse the pipeline. I was looking for such a thing in the API but didn't find it.

The careful reader notices two **Seq** measurements. **Seq2** contains a modified version of `Seq.upto` that doesn't allocate objects needlessly. This improves performance by approxmiately 4 times and reduces the memory pressure. Perhaps becomes a PR for F# later.

Finally, **SeqComposer** is taken from the PR that seeks to replace **Seq**. It improves **Seq** performance by roughly an order of magnitude which is a major improvement. **Nessos.Streams** still performs better for this pipeline though.

Mårten
