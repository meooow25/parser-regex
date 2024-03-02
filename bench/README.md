## Comparison

A comparison of some Haskell regex libraries:

* `parser-regex`
* [`regex-applicative`](https://hackage.haskell.org/package/regex-applicative)
* [`regex-tdfa`](https://hackage.haskell.org/package/regex-tdfa)
* [`regex-pcre-builtin`](https://hackage.haskell.org/package/regex-pcre-builtin)

| | `parser-regex` | `regex-applicative` | `regex-tdfa` | `regex-pcre-builtin` |
| --- | --- | --- | --- | --- |
| Regex construction | Combinators | Combinators | Pattern | Pattern |
| Unicode aware | Yes | Yes | Yes | No |
| Parsing features | Yes | Yes | Submatch only | Submatch only |
| Extensions | No | No | No | Yes (lookahead, backreferences, etc.) |
| Text matching speed<br/>(`English text 1,2`) | Baseline | Slower | Faster | Very fast |
| Text replace speed<br/>(`English replace all`) | Baseline | Slower | Slower <sup>[1]</sup> | Comparable <sup>[2]</sup> |
| Parsing speed<br/>(`CaseFolding.txt`,`URI`) | Baseline | Slower | Comparable | Very fast |
| Regex compilation complexity | $O(m)$ | Undocumented, $O(m^2)$ judging by source code | Undocumented | Undocumented |
| Parsing complexity | $O(mn \log m)$ | Documented "roughly linear complexity", $O(m^2 n \log m)$ judging by source code | $O(n)$ claimed <sup>[3]</sup> | Undocumented, $O(2^n)$ seen experimentally |

<sup>1</sup> Replacement requires a separate library, [`regex`](https://hackage.haskell.org/package/regex)  
<sup>2</sup> Replacement requires a separate library, [`regex-with-pcre`](https://hackage.haskell.org/package/regex-with-pcre)  
<sup>3</sup> I do not know if this is accurate, since $O(n)$ is only possible by spending
   $O(2^m)$ on compilation, which libraries usually consider too great a cost.
   `regex-tdfa` mentions that it is based on the [`tre`](https://github.com/laurikari/tre/)
   library, which claims $O(m^2 n)$ time. This could be true of `regex-tdfa` also.

## Benchmarks

Benchmarks of regex libraries on some simple use cases. See `Compare.hs` for
details.  
Performed using GHC 9.8.1.

The suffixes indicate the sequence used for the benchmarks, `T` for `Text`,
`S` for `String`, `BS` for `ByteString`.

### English text 1

| Library | Time | Alloc | Copied | Peak |
| --- | --- | --- | --- | --- |
| parser-regex T | 55.6 ms ± 2.8 ms | 379 MB | 91 KB | 27 MB |
| parser-regex S | 72.2 ms ± 2.9 ms | 366 MB | 90 KB | 27 MB |
| regex-applicative S | 371 ms ± 27 ms | 1.6 GB | 286 MB | 158 MB |
| regex-tdfa T | 38.6 ms ± 1.5 ms | 110 MB | 63 KB | 27 MB |
| regex-pcre-builtin BS | 13.4 ms ± 698 μs | 406 KB | 8.4 KB | 27 MB |

### English text 2

| Library | Time | Alloc | Copied | Peak |
| --- | --- | --- | --- | --- |
| parser-regex T | 49.6 ms ± 2.8 ms | 315 MB | 102 KB | 27 MB |
| parser-regex S | 49.6 ms ± 2.8 ms | 319 MB | 891 KB | 28 MB |
| regex-applicative S | 379 ms ± 15 ms | 2.1 GB | 284 MB | 217 MB |
| regex-tdfa T | 27.2 ms ± 1.4 ms | 112 MB | 33 KB | 28 MB |
| regex-pcre-builtin BS | 388 μs ± 30 μs | 298 KB | 3.1 KB | 27 MB |

### English replace all

| Library | Time | Alloc | Copied | Peak |
| --- | --- | --- | --- | --- |
| parser-regex T | 81.4 ms ± 2.9 ms | 357 MB | 65 MB | 73 MB |
| parser-regex S | 121 ms ± 5.9 ms | 396 MB | 100 MB | 93 MB |
| regex-applicative S | 368 ms ± 5.3 ms | 2.2 GB | 63 MB | 50 MB |
| regex-tdfa T | 200 ms ± 17 ms | 696 MB | 29 MB | 923 MB |
| regex-pcre-builtin BS | 122 ms ± 12 ms | 586 MB | 29 MB | 921 MB |

### Parse CaseFolding.txt

| Library | Time | Alloc | Copied | Peak |
| --- | --- | --- | --- | --- |
| parser-regex T | 60.1 ms ± 3.9 ms | 324 MB | 2.2 MB | 14 MB |
| parser-regex S | 60.1 ms ± 3.6 ms | 321 MB | 1.7 MB | 11 MB |
| regex-applicative S | 143 ms ± 4.2 ms | 921 MB | 48 MB | 65 MB |
| regex-tdfa T | 39.7 ms ± 1.3 ms | 108 MB | 168 KB | 11 MB |
| regex-pcre-builtin BS | 12.3 ms ± 683 μs | 4.0 MB | 402 KB | 11 MB |

### Parse URI

| Library | Time | Alloc | Copied | Peak |
| --- | --- | --- | --- | --- |
| parser-regex T | 70.5 ms ± 3.8 ms | 454 MB | 3.3 MB | 33 MB |
| parser-regex S | 80.9 ms ± 3.4 ms | 439 MB | 9.5 MB | 35 MB |
| regex-applicative S | 426 ms ± 20 ms | 2.1 GB | 284 MB | 165 MB |
| regex-tdfa T | 192 ms ± 3.4 ms | 246 MB | 579 KB | 32 MB |
| regex-pcre-builtin BS | 7.13 ms ± 259 μs | 13 MB | 9.2 MB | 48 MB |

### Exponential backtracking

| Library | Time | Alloc | Copied | Peak |
| --- | --- | --- | --- | --- |
| parser-regex T | 17.1 μs ± 1.3 μs | 95 KB | 135 B | 7.0 MB |
| parser-regex S | 16.8 μs ± 1.4 μs | 99 KB | 137 B | 7.0 MB |
| regex-applicative S | 20.5 μs ± 893 ns | 44 KB | 8 B | 7.0 MB |
| regex-tdfa T | 401 ns ± 22 ns | 3.6 KB | 0 B | 9.0 MB |
| regex-pcre-builtin BS | 160 ms ± 6.6 ms | 0 B | 0 B | 6.0 MB |
