# Performance comparison of different data pipelines in F#

As F# developers we all know about **Seq** that allows to write a data pipelines:

```fsharp
let result =
  Seq.init count id
  |> Seq.map    (fun n -> int64 n * 5L)
  |> Seq.filter (fun n -> n % 7L <> 0L)
  |> Seq.map    (fun n -> n / 11L)
  |> Seq.sum
```

There are many data pipelines to chose from.

0. **Imperative** - If we weren't F# developers we would use imperative patterns to implement the data pipeline about.
1. **Seq** - Good old **Seq**
2. **Linq** - **Linq** with some F# idiomatic wrappers is totally ok
3. **Nessos.Streams** - The gurus at *Nessos* created a data pipeline that supports push, pull, parallelism, cancelability and so on. It's also supposed to be really fast
4. **Nessos.LinqOptimizer** - While **Nessos.Streams** is cool, compared to **Nessos.LinqOptimizer** it pales as the later optimizes the data pipeline when constructed to improve performance.
5. **SeqComposer** - There's a PR in F# repository that intends to improve performance of **Seq**.
6. **PullStream** - I implemented a simple pull (like **Seq**) data pipeline to compare against
7. **PushStream** - I implemented a simple push (like supported by **Nessos.Streams**) data pipeline to compare against
8. **PushPipe** - A variant of **PushStream** intended to reduce the overhead of creating data pipelines.

I asked myself; *which data pipeline has the lowest overhead?*

In order to test this I created a simple pipeline that I implemented for all variants above. I was inspired by a data pipeline they used to test **SeqComposer** PR.

```fsharp
let simple () =
  range count
  |> map    (fun n -> int64 n * 5L)
  |> filter (fun n -> n % 7L <> 0L)
  |> map    (fun n -> n / 11L)
  |> sum
```

As we measure the overhead of the data pipeline itself we keep the work in the lambda expressions small.

As the source is a simple range and the result is a sum the memory consumption is a result of the data pipeline.

