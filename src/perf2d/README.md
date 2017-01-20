# On the topic of 2D array performance in .NET

*[Full source code can be found here](https://github.com/mrange/fsharpadvent2016/tree/master/src/perf2d)*

It popped up a discussion in FSSF Slack over 2D arrays and performance. I thought it could be good to share some performance numbers for different alternatives in .NET.

For this I am computing the Matrix product:

```
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

I use 3 variants of Matrix representation:

0. **float[,]**   - The obvious representation of a Matrix is a 2D array
0. **float[][]**  - Another obvious representation of a Matrix is a so called jagged array or array of array
0. **float[]**    - Slightly less obvious is to use a 1D array with enough space to hold all matrix values and then we compute the index into the array like this `row*row_width + column`.

Without further ado here are the results.

## Performance in Milliseconds - F# 4, .NET 4.6.1, x64

![Performance in Milliseconds - F# 4, .NET 4.6.2, x64](http://i.imgur.com/Cz2rRG6.png)

*Note that the scale is logarithmic*

From the results we see that **float[,]** performs the worst in general. This is because that the jitted code does a lot of work when dereferencing a 2D array, like for instance taking into account the base index for each axis (which is 0 in my case) . For 1D arrays the jitted code is a lot more efficient.

In most cases the **naive** version does better on smaller cases. This is because the code dereferences the result array less often.

For larger Matrix size the performance of the **naive** version degrades a lot because that when `i` change in each iteration `b.[i, col]` will cause cache misses.

The **better** version the loops are changed so that `a.[row, i]` is kept constant in the inner loop that iterates of `col` leading to better locality for `b.[i, col]`.

**float[][]** **naive** version is doing the worst for large matrix sizes but **float[][]** **better** version does best overall.

I hope you found this interesting.

Mårten



