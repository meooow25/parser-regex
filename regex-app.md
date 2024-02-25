## Comparison with regex-applicative

### Things from `regex-applicative` adopted in `parser-regex`

1. Regex in the style of parser combinators. An excellent idea.
2. Works on a sequence of any type. Regexes in literature and implementations
   in the wild usually assume a fixed small alphabet. Being polymorphic here is
   fitting for Haskell, and nothing is assumed about the alphabet.
3. Replacement using `RE [c] [c]`. Neat.
4. The `withMatched` combinator. Also neat.

### `regex-applicative`'s shortcomings, as I see them

1. `regex-applicative` only provides an API to work on "unconsables". This is
   overly restrictive because the algorithm works by simply feeding tokens in
   one-by-one.

   `parser-regex` works on lists, `Text` and `ByteString`
   out-of-the-box, and provides lower-level functions to define regex parsers
   for any sequence type.
2. `regex-applicative` uses unusual terminology (`sym`, `psym`, `msym`), for
   reasons unknown to me.

   `parser-regex` uses `parsec` terminology (`satisfy`, `token`).
3. The `Filterable RE` instance in `regex-applicative` has surprising semantics.
  `filter f ((a <|> b) <* c)` is not the same as
  `filter f ((a <* c) <|> (b <* c))`.
   For example:

   | Regex | Input | Result |
   |---|---|---|
   | `filter even ((1 <$ sym 'a' <\|> 2 <$ sym 'a') <* sym 'b')` | `"ab"` | `Nothing` |
   | `filter even ((1 <$ sym 'a' <* sym 'b' <\|> 2 <$ sym 'a' <* sym 'b'))` | `"ab"` | `Just 2` |

   `parser-regex`'s `Re` is not `Filterable`.
4. `regex-applicative` does not handle loopy parsers well. Some
   examples (not exhaustive):

   | Regex | Input | `regex-applicative` | Correct? | `parser-regex` |
   |---|---|---|---|---|
   | `many (pure 1)` | `""` | `Just []` | ❌ | `Just (repeat 1)` |
   | `many (pure 1)` | `"a"` | `Nothing` | ✅ | `Nothing` |
   | `many (void (pure 1))` | `""` | Hangs | ⚠️ | `Just (repeat ())` |
   | `many ((,) <$> pure 1 <*> pure 2)` | `""` | `Just []` | ❌ | `Just (repeat (1,2))` |
   | `many (pure 1 *> pure 2)` | `""` | Hangs | ⚠️ | `Just (repeat 2)` |
   | `many ((,) <$> pure 1 <*> pure 2)` | `"a"` | `Nothing` | ✅ | `Nothing` |
   | `many (pure 1 *> pure 2)` | `"a"` | Hangs | ⚠️ | `Nothing` |
   | `many (few anySym)` | `""` | `Just []` | ❌ | `Just (repeat "")` |
   | `many (void (few anySym))` | `""` | Hangs | ⚠️ | `Just (repeat ())` |

   It is unlikely someone would write a regex like this. However, `parser-regex`
   handles these correctly because:
   1. It is good to be correct
   2. The regex could be built from an untrusted source and we would't want it
      to DoS the program.

5. `regex-applicative`'s `match` performs both compilation and parsing, and the
   recommended way to compile and parse multiple times is partially applying the
   function to a regex. This is, however, easy to miss and can lead to issues like
   https://github.com/UnkindPartition/regex-applicative/issues/49

   `parser-regex` offers `compile` and `parse` as separate functions, and
   `parseCompile` to perform both at once.

### Performance comparision

* `parser-regex` and `regex-applicative` use the same technique and have the
  same time complexity for parsing and recognition, $O(m^2 n)$.
* `parser-regex`'s recognition runs in $O(m)$ memory. For parsing, memory usage
  depends on the regex and is $O(m^2)$ in the worst case. Neither depends on
  the sequence length $n$. `regex-applicative` requires $O(mn)$ memory
  for recognition and $O(m^2n)$ for parsing. See benchmarks.
* In practice, `parser-regex` takes 1/x of the time taken by `regex-applicative`
  for parsing. `parser-regex` takes 1/1 if the time taken by `regex-applicative`
  for recognition. See benchmarks for more details.

## my todo

[x] figure out loops and eps transitions
[x] add list api
[x] add more tests
[x] check coverage of loopy parsers
[x] add integer parsing
[x] add int parsing
[x] add some benchmarks
[x] add realistic comparison benchmarks
[x] recommend base combinators: replicateM, replicateM_, optional, asum
[x] add count
[x] add simple case fold
[x] add signed integer
[ ] flag for pedantic checks
[ ] readme and diffs
