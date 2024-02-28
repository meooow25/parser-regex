# parser-regex

Regex based parsers

## Features

* Parsers based on [regular expressions](https://en.wikipedia.org/wiki/Regular_expression),
  capable of parsing [regular languages](https://en.wikipedia.org/wiki/Regular_language).
  There are no extra features that would make parsing non-regular languages
  possible.
* Regexes are composed using combinators.
* Resumable parsing of sequences of any type containing values of any type.
* Special support for `Text` and `String` in the form of convenient combinators
  and operations like find and replace.
* Parsing runtime is linear in the length of the sequence being parsed. No
  exponential backtracking.

## Usage

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
-- Translated:
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

### `regex-applicative`

[`regex-applicative`](https://hackage.haskell.org/package/regex-applicative) is
the primary inspiration for this library, and provides a similar set of
features.
`parser-regex` attempts to be a more fully-featured library built on the
ideas of `regex-applicative`.

### Traditional regex libraries

Other alternatives are more traditional regex libraries that use regex patterns,
like [`regex-tdfa`](https://hackage.haskell.org/package/regex-tdfa) and
[`regex-pcre`](https://hackage.haskell.org/package/regex-pcre)/
[`regex-pcre-builtin`](https://hackage.haskell.org/package/regex-pcre-builtin).

Reasons to use `parser-regex` over traditional regex libraries:

* You prefer parser combinators over regex patterns
* You need more powerful parsing capabilities than just submatch extraction
* You need to parse a sequence type that is not supported by these regex
  libraries

Reasons to use traditional regex libraries over `parser-regex`:

* The terseness of regex patterns is better suited for your use case
* You need something very fast, and adversarial input is not a concern.
  Use `regex-pcre`/`regex-pcre-builtin`.

For a more detailed comparison of regex libraries, see
[here](https://github.com/meooow25/parser-regex/tree/master/bench).

## Contributing

Questions, bug reports, documentation improvements, code contributions welcome!
Please [open an issue](https://github.com/meooow25/parser-regex/issues) as the
first step.
