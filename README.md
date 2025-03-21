# parser-regex

[![Hackage](https://img.shields.io/hackage/v/parser-regex?logo=haskell&color=blue)](https://hackage.haskell.org/package/parser-regex)
[![Haskell-CI](https://github.com/meooow25/parser-regex/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/meooow25/parser-regex/actions/workflows/haskell-ci.yml)

Regex based parsers

## Features

* Parsers based on [regular expressions](https://en.wikipedia.org/wiki/Regular_expression),
  capable of parsing [regular languages](https://en.wikipedia.org/wiki/Regular_language).
  Note that there are no extra features to make parsing non-regular languages
  possible.
* Regexes are composed using combinators.
* Resumable parsing of sequences of any type containing values of any type.
* Special support for `Text` and `String` in the form of convenient combinators
  and operations like find and replace.
* Parsing runtime is linear in the length of the sequence being parsed. No
  exponential backtracking.

## Examples

### Versus regex patterns

```
^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
```

Can you guess what this matches?

This is a non-validating regex to extract parts of a URI, from
[RFC 3986](https://datatracker.ietf.org/doc/html/rfc3986#appendix-B). It can
be translated as follows.

```hs
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative (optional)
import Data.Text (Text)

import Regex.Text (REText)
import qualified Regex.Text as R
import qualified Data.CharSet as CS

data URI = URI
  { scheme    :: Maybe Text
  , authority :: Maybe Text
  , path      :: Text
  , query     :: Maybe Text
  , fragment  :: Maybe Text
  } deriving Show

uriRE :: REText URI
uriRE = URI
  <$> optional (R.someTextOf (CS.not ":/?#") <* R.char ':')
  <*> optional (R.text "//" *> R.manyTextOf (CS.not "/?#"))
  <*> R.manyTextOf (CS.not "?#")
  <*> optional (R.char '?' *> R.manyTextOf (CS.not "#"))
  <*> optional (R.char '#' *> R.manyText)
```
```hs
>>> R.reParse uriRE "https://github.com/meooow25/parser-regex?tab=readme-ov-file#parser-regex"
Just (URI { scheme = Just "https"
          , authority = Just "github.com"
          , path = "/meooow25/parser-regex"
          , query = Just "tab=readme-ov-file"
          , fragment = Just "parser-regex" })
```

### More parsing

Parsing is straightforward, even for tasks which may be impractical with
submatch extraction typically offered by regex libraries.

```hs
import Control.Applicative ((<|>))
import Data.Text (Text)

import Regex.Text (REText)
import qualified Regex.Text as R
import qualified Data.CharSet as CS

data Expr
  = Var Text
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  deriving Show

exprRE :: REText Expr
exprRE = var `R.chainl1` mul `R.chainl1` (add <|> sub)
  where
    var = Var <$> R.someTextOf CS.asciiLower
    add = (:+) <$ R.char '+'
    sub = (:-) <$ R.char '-'
    mul = (:*) <$ R.char '*'
```
```hs
>>> import qualified Regex.Text as R
>>> R.reParse exprRE "a+b-c*d*e+f"
Just (((Var "a" :+ Var "b") :- ((Var "c" :* Var "d") :* Var "e")) :+ Var "f")
```

### Find and replace

Find and replace using regexes are supported for `Text` and lists.

```hs
>>> import Control.Applicative ((<|>))
>>> import qualified Data.Text as T
>>> import qualified Regex.Text as R
>>>
>>> data Color = Blue | Orange deriving Show
>>> let re = Blue <$ R.text "blue" <|> Orange <$ R.text "orange"
>>> R.find re "color: orange"
Just Orange
>>>
>>> let re = T.toUpper <$> (R.text "cat" <|> R.text "dog" <|> R.text "fish")
>>> R.replaceAll re "locate selfish hotdog"
"loCATe selFISH hotDOG"
```

### Parse any sequence

Regexes are not restricted to parsing text. For example, one may parse vectors
from the [vector](https://hackage.haskell.org/package/vector) library, because
why not.

```hs
import Regex.Base (Parser)
import qualified Regex.Base as R
import qualified Data.Vector.Generic as VG

parseVector :: VG.Vector v c => Parser c a -> v c -> Maybe a
parseVector = R.parseFoldr VG.foldr
```
```hs
>>> import Control.Applicative (many)
>>> import qualified Data.Vector as V
>>> import qualified Regex.Base as R
>>>
>>> let p = R.compile $ many ((,) <$> R.satisfy even <*> R.satisfy odd)
>>> let v = V.fromList [0..5] :: V.Vector Int
>>> parseVector p v
Just [(0,1),(2,3),(4,5)]
```

## Documentation

Documentation is available on Hackage:
[parser-regex](https://hackage.haskell.org/package/parser-regex)

Already familiar with regex patterns? See the
[Regex pattern cheat sheet](https://github.com/meooow25/parser-regex/wiki/Regex-pattern-cheat-sheet).

## Alternatives

### `regex-applicative`

[`regex-applicative`](https://hackage.haskell.org/package/regex-applicative) is
the primary inspiration for this library, and is similar in many ways.

`parser-regex` attempts to be a more efficient and featureful library built on
the ideas of `regex-applicative`, though it does not aim to provide a superset
of `regex-applicative`'s API.

### Traditional regex libraries

These libraries use regex patterns.

* [`regex-pcre`](https://hackage.haskell.org/package/regex-pcre)/[`regex-pcre-builtin`](https://hackage.haskell.org/package/regex-pcre-builtin)
* [`regex-tdfa`](https://hackage.haskell.org/package/regex-tdfa)
* [`pcre-light`](https://hackage.haskell.org/package/pcre-light)/[`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy)
* [`pcre2`](https://hackage.haskell.org/package/pcre2)

Consider using these if

* The terseness of regex patterns is well-suited for your use case.
* You need something very fast for typical use cases. `regex-pcre`,
  `regex-pcre-builtin`, `pcre-light`, `pcre-heavy` are faster than
  `parser-regex` for typical use cases, but there are trade-offs—such as losing
  Unicode support and a risk of [ReDoS](https://en.wikipedia.org/wiki/ReDoS).

Use `parser-regex` instead if

* You prefer parser combinators over regex patterns
* You need more powerful parsing capabilities than just submatch extraction
* You need to parse a sequence that is not supported by the above libraries

For a detailed comparison of regex libraries,
[see here](https://github.com/meooow25/parser-regex/tree/master/bench).

### Other options

If you are not restricted to regexes, there are many other parsing libraries you
may use, too many to list here. See the
["Parsing" category on Hackage](https://hackage.haskell.org/packages/#cat:Parsing)
for a start.

## Contributing

Questions, bug reports, documentation improvements, code contributions welcome!
Please [open an issue](https://github.com/meooow25/parser-regex/issues) as the
first step.
