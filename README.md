# parser-regex

Regex based parsers

## Features

* Parsers based on [regular expressions](https://en.wikipedia.org/wiki/Regular_expression),
  capable of parsing [regular languages](https://en.wikipedia.org/wiki/Regular_language).
  There are no extra features that would make parsing non-regular languages
  possible.

* Regexes are composed together using combinators. They are not represented by
  an inscrutable string of symbols.

* Parse sequences of any type containing values of any type.
  Special support is provided for `Text` and `String` in the form of additional
  combinators and operations like find and replace.

* Parsing runtime is linear in the length of the sequence being parsed. No
  exponential backtracking.

## Example

```hs
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative (optional)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Regex.Text as R
import qualified Data.CharSet as CS

data URI = URI
  { scheme    :: Maybe Text
  , authority :: Maybe Text
  , path      :: Text
  , query     :: Maybe Text
  , fragment  :: Maybe Text
  } deriving Show

-- ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
-- A non-validating regex to extract parts of a URI, from RFC 3986
-- The equivalent of this regex:
uriRE :: R.REText URI
uriRE = URI
  <$> optional (R.someTextOf (CS.not ":/?#") <* R.char ':')
  <*> optional (R.text "//" *> R.manyTextOf (CS.not "/?#"))
  <*> R.manyTextOf (CS.not "?#")
  <*> optional (R.char '?' *> R.manyTextOf (CS.not "#"))
  <*> optional (R.char '#' *> R.manyText)

-- >>> R.reParse uriRE "https://github.com/meooow25/parser-regex?tab=readme-ov-file#parser-regex"
-- Just (URI { scheme = Just "https"
--           , authority = Just "github.com"
--           , path = "/meooow25/parser-regex"
--           , query = Just "tab=readme-ov-file"
--           , fragment = Just "parser-regex" })
```

## Alternatives

[`regex-applicative`](https://hackage.haskell.org/package/regex-applicative) is
the primary inspiration for this package, and provides a similar set of
features.
`parser-regex` attempts to be a more fully-featured library built on the
ideas of `regex-applicative`.

Other alternatives are more traditional regex libraries, like
[`regex-tdfa`](https://hackage.haskell.org/package/regex-tdfa) and
[`regex-pcre`](https://hackage.haskell.org/package/regex-pcre)/
[`regex-pcre-builtin`](https://hackage.haskell.org/package/regex-pcre-builtin).

It is also common to reach for monadic parser combinator libraries, but
comparing them with regexes is comparing apples and oranges. However, it may be
what you need, so please see
[`parsec`](https://hackage.haskell.org/package/parsec),
[`megaparsec`](https://hackage.haskell.org/package/megaparsec), or
[`attoparsec`](https://hackage.haskell.org/package/attoparsec).

### Performance

`parser-regex` is not going to be the fastest regex library on Hackage. Because
of the polymorphic design of the library, it cannot compete with specialized
regex libraries with a smaller feature set. Of course, effort has been made
to be as fast as possible. Any ideas for improving performance further are very
welcome.

## Contributing

Questions, bug reports, documentation improvements, code contributions welcome!
Please [open an issue](https://github.com/meooow25/parser-regex/issues) as the
first step.
