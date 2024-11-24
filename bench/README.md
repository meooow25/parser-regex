## Comparison

A comparison of some Haskell regex libraries:

* `parser-regex`
* [`regex-applicative`](https://hackage.haskell.org/package/regex-applicative)
* [`regex-tdfa`](https://hackage.haskell.org/package/regex-tdfa)
* [`regex-pcre-builtin`](https://hackage.haskell.org/package/regex-pcre-builtin)
* [`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy)
* [`pcre2`](https://hackage.haskell.org/package/pcre2)

| | `parser-regex` | `regex-applicative` | `regex-tdfa` | `regex-pcre-builtin` | `pcre-heavy` | `pcre2` |
| --- | --- | --- | --- | --- | --- | --- |
| Regex construction | Combinators | Combinators | Pattern | Pattern | Pattern | Pattern |
| Unicode aware | Yes | Yes | Yes | No<sup>[1]</sup> | No | Yes |
| Parsing features | Yes | Yes | Submatch only | Submatch only | Submatch only | Submatch only |
| Extensions | No | No | No | Yes<sup>[2]</sup> | Yes<sup>[2]</sup> | Yes<sup>[2]</sup> |
| Text matching speed<br/>(`English text 1,2`) | Baseline | Very slow | Fast | Very fast | Very fast | Slow |
| Text replace speed<br/>(`English replace all`) | Baseline | Slow | Slow<sup>[3]</sup> | Comparable<sup>[4]</sup> | Very fast | Fast |
| Parsing speed<br/>(`CaseFolding.txt`,`URI`) | Baseline | Slow | Comparable | Very fast | Very fast | ⚠ UTF-8 error |
| Regex compilation complexity | $O(m)$ | $O(m^2)$ judging by source code | Unclear | Unclear | Unclear | Unclear |
| Parsing complexity | $O(mn \log m)$ | $O(m^2 n \log m)$ judging by source code | $O(n)$ claimed<sup>[4]</sup> | $O(2^n)$ seen experimentally | $O(2^n)$ seen experimentally | $O(2^n)$ seen experimentally |

<sup>1</sup> [`regex-pcre-builtin#3`](https://github.com/audreyt/regex-pcre-builtin/issues/3)  
<sup>2</sup> Supports lookahead, backreferences, etc.  
<sup>3</sup> Replacement requires a separate library, [`regex`](https://hackage.haskell.org/package/regex)  
<sup>4</sup> Replacement requires a separate library, [`regex-with-pcre`](https://hackage.haskell.org/package/regex-with-pcre)  
<sup>5</sup> This is unlikely to be true, since $O(n)$ is only possible by
   spending $O(2^m)$ on compilation, which libraries usually consider too great
   a cost. `regex-tdfa` mentions that it is based on the [`tre`](https://github.com/laurikari/tre/)
   library, which claims $O(m^2 n)$ time. This may be true of `regex-tdfa` also.

Classifications (time): <0.25x = Very fast, >0.25x and <0.5x = Fast, >0.5x
and <2x = Comparable, >2x and <4x = Slow, time >4x = Very slow

## Benchmarks

Benchmarks of regex libraries on some simple use cases. See `Compare.hs` for
details.  
Performed using GHC 9.10.1 on x86-64.

The suffixes indicate the sequence used for the benchmarks, `T` for `Text`,
`S` for `String`, `BS` for `ByteString`.

#### English text 1

```
┌────────────────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │ Copied │  Peak  │
│───────────────────────│─────────│────────│────────│────────│
│ parser-regex T        │ 59.3 ms │ 380 MB │  79 KB │  27 MB │
│ parser-regex S        │ 83.2 ms │ 368 MB │  83 KB │  27 MB │
│ regex-applicative S   │ 385  ms │ 1.6 GB │ 286 MB │ 157 MB │
│ regex-tdfa T          │ 38.2 ms │ 111 MB │  11 KB │  27 MB │
│ regex-pcre-builtin BS │ 13.4 ms │ 708 KB │ 598 B  │  27 MB │
│ pcre-heavy T          │ 13.5 ms │ 1.3 MB │ 620 B  │  27 MB │
│ pcre2 T               │ 228  ms │ 1.9 MB │ 2.5 KB │  27 MB │
└────────────────────────────────────────────────────────────┘
```

#### English text 2


```
┌────────────────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │ Copied │  Peak  │
│───────────────────────│─────────│────────│────────│────────│
│ parser-regex T        │ 50.6 ms │ 315 MB │  96 KB │  27 MB │
│ parser-regex S        │ 52.0 ms │ 320 MB │ 974 KB │  28 MB │
│ regex-applicative S   │ 399  ms │ 2.1 GB │ 284 MB │ 239 MB │
│ regex-tdfa T          │ 26.4 ms │ 113 MB │  12 KB │  28 MB │
│ regex-pcre-builtin BS │ 378  μs │ 277 KB │ 104 B  │  27 MB │
│ pcre-heavy T          │ 418  μs │ 788 KB │ 102 B  │  27 MB │
│ pcre2 T               │ 96.4 ms │ 1.0 MB │ 2.4 KB │  27 MB │
└────────────────────────────────────────────────────────────┘
```

#### English replace all

```
┌────────────────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │ Copied │  Peak  │
│───────────────────────│─────────│────────│────────│────────│
│ parser-regex T        │ 84.0 ms │ 358 MB │  65 MB │  73 MB │
│ parser-regex S        │ 121  ms │ 396 MB │  93 MB │  90 MB │
│ regex-applicative S   │ 386  ms │ 2.2 GB │  63 MB │  40 MB │
│ regex-tdfa T          │ 187  ms │ 696 MB │  26 MB │ 758 MB │
│ regex-pcre-builtin BS │ 125  ms │ 586 MB │  44 MB │ 490 MB │
│ pcre-heavy T          │ 22.7 ms │ 378 MB │  13 KB │  28 MB │
│ pcre2 T               │ 36.0 ms │ 1.6 MB │ 2.1 KB │  27 MB │
└────────────────────────────────────────────────────────────┘
```

#### Parse CaseFolding.txt

```
┌────────────────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │ Copied │  Peak  │
│───────────────────────│─────────│────────│────────│────────│
│ parser-regex T        │ 58.7 ms │ 325 MB │ 1.8 MB │  11 MB │
│ parser-regex S        │ 59.8 ms │ 322 MB │ 2.3 MB │  13 MB │
│ regex-applicative S   │ 146  ms │ 922 MB │  48 MB │  67 MB │
│ regex-tdfa T          │ 40.6 ms │ 108 MB │  83 KB │  11 MB │
│ regex-pcre-builtin BS │ 12.1 ms │ 4.3 MB │  63 KB │  10 MB │
│ pcre-heavy T          │ 12.4 ms │ 5.2 MB │ 1.2 KB │  10 MB │
│ pcre2 T               │ 115  ms │ 9.7 MB │ 6.0 KB │  10 MB │
└────────────────────────────────────────────────────────────┘
```

#### Parse URI

```
┌────────────────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │ Copied │  Peak  │
│───────────────────────│─────────│────────│────────│────────│
│ parser-regex T        │ 73.8 ms │ 455 MB │ 3.9 MB │  34 MB │
│ parser-regex S        │ 86.6 ms │ 441 MB │  11 MB │  38 MB │
│ regex-applicative S   │ 451  ms │ 2.1 GB │ 288 MB │ 162 MB │
│ regex-tdfa T          │ 193  ms │ 246 MB │ 100 KB │  31 MB │
│ regex-pcre-builtin BS │ 6.52 ms │  12 MB │ 7.3 MB │  46 MB │
│ pcre-heavy T          │ 3.84 ms │  15 MB │ 385 KB │  59 MB │
└────────────────────────────────────────────────────────────┘
```

#### Exponential backtracking

```
┌────────────────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │ Copied │  Peak  │
│───────────────────────│─────────│────────│────────│────────│
│ parser-regex T        │ 18.5 μs │  96 KB │  88 B  │ 6.0 MB │
│ parser-regex S        │ 18.0 μs │ 100 KB │  89 B  │ 6.0 MB │
│ regex-applicative S   │ 22.9 μs │  44 KB │  89 B  │ 7.0 MB │
│ regex-tdfa T          │ 2.37 μs │ 3.6 KB │  88 B  │ 8.0 MB │
│ regex-pcre-builtin BS │ 161  ms │ 563 KB │ 7.6 KB │ 6.0 MB │
│ pcre-heavy T          │ 160  ms │ 562 KB │  51 KB │ 6.0 MB │
│ pcre2 T               │ 257  ms │ 114 KB │  49 KB │ 6.0 MB │
└────────────────────────────────────────────────────────────┘
```
