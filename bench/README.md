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
| Text matching speed<br/>(`English text 1, 2`) | 🟡 Baseline | 🔴🔴 | 🟡 | 🟢🟢 | 🟢🟢 | 🔴 |
| Text replace speed<br/>(`English replace all`) | 🟡 Baseline | 🔴 | 🔴<sup>[3]</sup> | 🟡<sup>[4]</sup> | 🟢 | 🟢 |
| Parsing speed<br/>(`CaseFolding.txt`,`URI`) | 🟡 Baseline | 🔴🔴 | 🔴 | 🟢🟢 | 🟢🟢 | 🔴🔴 |
| Regex compilation complexity | $O(m)$ | $O(m^2)$ judging by source code | Unclear | Unclear | Unclear | Unclear |
| Parsing complexity | $O(mn \log m)$ | $O(m^2 n \log m)$ judging by source code | $O(n)$ claimed<sup>[5]</sup> | $O(2^n)$ seen experimentally | $O(2^n)$ seen experimentally | $O(2^n)$ seen experimentally |

<sup>1</sup> [`regex-pcre-builtin#3`](https://github.com/audreyt/regex-pcre-builtin/issues/3)  
<sup>2</sup> Supports lookahead, backreferences, etc.  
<sup>3</sup> Replacement requires a separate library, [`regex`](https://hackage.haskell.org/package/regex)  
<sup>4</sup> Replacement requires a separate library, [`regex-with-pcre`](https://hackage.haskell.org/package/regex-with-pcre)  
<sup>5</sup> This is unlikely to be true, since $O(n)$ is only possible by
   spending $O(2^m)$ on compilation, which libraries usually consider too great
   a cost. `regex-tdfa` is based on the [`tre`](https://github.com/laurikari/tre/)
   library which claims $O(m^2 n)$ time. It is more reasonable that the same
   applies to `regex-tdfa`.

Classifications (time): <0.25x = 🟢🟢, >0.25x and <0.5x = 🟢, >0.5x and
<2x = 🟡, >2x and <4x = 🔴, >4x = 🔴🔴

## Benchmarks

Benchmarks of regex libraries on some simple use cases. See `Compare.hs` for
details.  
Performed using GHC 9.10.1 on x86-64.

The suffixes indicate the sequence used for the benchmarks, `T` for `Text`,
`S` for `String`, `BS` for `ByteString`.

#### English text 1

```
┌───────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │  Peak  │
│───────────────────────│─────────│────────│────────│
│ parser-regex T        │ 47.3 ms │ 207 MB │  27 MB │
│ parser-regex S        │ 64.8 ms │ 193 MB │  27 MB │
│ regex-applicative S   │ 385  ms │ 1.6 GB │ 157 MB │
│ regex-tdfa T          │ 38.2 ms │ 111 MB │  27 MB │
│ regex-pcre-builtin BS │ 13.4 ms │ 708 KB │  27 MB │
│ pcre-heavy T          │ 13.5 ms │ 1.3 MB │  27 MB │
│ pcre2 T               │ 183  ms │ 2.0 MB │  27 MB │
└───────────────────────────────────────────────────┘
```

#### English text 2


```
┌───────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │  Peak  │
│───────────────────────│─────────│────────│────────│
│ parser-regex T        │ 39.5 ms │ 194 MB │  27 MB │
│ parser-regex S        │ 39.0 ms │ 198 MB │  27 MB │
│ regex-applicative S   │ 399  ms │ 2.1 GB │ 239 MB │
│ regex-tdfa T          │ 26.4 ms │ 113 MB │  28 MB │
│ regex-pcre-builtin BS │ 378  μs │ 277 KB │  27 MB │
│ pcre-heavy T          │ 418  μs │ 788 KB │  27 MB │
│ pcre2 T               │ 78.7 ms │ 1.1 MB │  27 MB │
└───────────────────────────────────────────────────┘
```

#### English replace all

```
┌───────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │  Peak  │
│───────────────────────│─────────│────────│────────│
│ parser-regex T        │ 74.1 ms │ 197 MB │  84 MB │
│ parser-regex S        │ 106  ms │ 235 MB │  90 MB │
│ regex-applicative S   │ 386  ms │ 2.2 GB │  40 MB │
│ regex-tdfa T          │ 187  ms │ 696 MB │ 758 MB │
│ regex-pcre-builtin BS │ 125  ms │ 586 MB │ 490 MB │
│ pcre-heavy T          │ 22.7 ms │ 378 MB │  28 MB │
│ pcre2 T               │ 38.7 ms │ 1.7 MB │  27 MB │
└───────────────────────────────────────────────────┘
```

#### Parse CaseFolding.txt

```
┌───────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │  Peak  │
│───────────────────────│─────────│────────│────────│
│ parser-regex T        │ 43.2 ms │ 219 MB │  11 MB │
│ parser-regex S        │ 45.7 ms │ 215 MB │  13 MB │
│ regex-applicative S   │ 146  ms │ 922 MB │  67 MB │
│ regex-tdfa T          │ 40.6 ms │ 108 MB │  11 MB │
│ regex-pcre-builtin BS │ 12.1 ms │ 4.3 MB │  10 MB │
│ pcre-heavy T          │ 12.4 ms │ 5.2 MB │  10 MB │
│ pcre2 T               │ 53.9 ms │  10 MB │  10 MB │
└───────────────────────────────────────────────────┘
```

#### Parse URI

```
┌───────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │  Peak  │
│───────────────────────│─────────│────────│────────│
│ parser-regex T        │ 53.5 ms │ 290 MB │  34 MB │
│ parser-regex S        │ 62.2 ms │ 276 MB │  38 MB │
│ regex-applicative S   │ 451  ms │ 2.1 GB │ 162 MB │
│ regex-tdfa T          │ 193  ms │ 246 MB │  31 MB │
│ regex-pcre-builtin BS │ 6.52 ms │  12 MB │  46 MB │
│ pcre-heavy T          │ 3.84 ms │  15 MB │  59 MB │
│ pcre2 T               │ 806  ms │  21 MB │  32 MB │
└───────────────────────────────────────────────────┘
```

#### Exponential backtracking

```
┌───────────────────────────────────────────────────┐
│                       │  Time   │ Alloc  │  Peak  │
│───────────────────────│─────────│────────│────────│
│ parser-regex T        │ 10.0 μs │  52 KB │ 6.0 MB │
│ parser-regex S        │ 9.55 μs │  58 KB │ 6.0 MB │
│ regex-applicative S   │ 22.9 μs │  44 KB │ 7.0 MB │
│ regex-tdfa T          │ 2.37 μs │ 3.6 KB │ 8.0 MB │
│ regex-pcre-builtin BS │ 161  ms │ 563 KB │ 6.0 MB │
│ pcre-heavy T          │ 160  ms │ 562 KB │ 6.0 MB │
│ pcre2 T               │ 259  ms │ 567 KB │ 6.0 MB │
└───────────────────────────────────────────────────┘
```
