{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This is an internal module. You probably don't need to import this. Import
-- "Regex.Text" instead.
--
-- = WARNING
--
-- Definitions in this module allow violating invariants that would otherwise be
-- guaranteed by non-internal modules. Use at your own risk!
--
module Regex.Internal.Text
  (
    TextToken(..)
  , REText
  , textTokenFoldr

  , token
  , satisfy
  , char
  , charIgnoreCase
  , anyChar
  , oneOf
  , text
  , textIgnoreCase
  , manyText
  , someText
  , manyTextMin
  , someTextMin
  , manyTextOf
  , someTextOf
  , manyTextOfMin
  , someTextOfMin

  , naturalDec
  , integerDec
  , naturalHex
  , integerHex
  , wordRangeDec
  , intRangeDec
  , wordRangeHex
  , intRangeHex
  , wordDecN
  , wordHexN

  , toMatch
  , withMatch

  , reParse
  , ParserText
  , parse
  , parseSure

  , find
  , findAll
  , splitOn
  , replace
  , replaceAll
  ) where

import Control.Applicative
import Data.Char
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import Numeric.Natural
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as TArray
import qualified Data.Text.Internal as TInternal
import qualified Data.Text.Unsafe as TUnsafe
import qualified Data.Text.Internal.Encoding.Utf8 as TInternalUtf8

import Data.CharSet (CharSet)
import qualified Data.CharSet as CS
import Regex.Internal.Parser (Parser)
import qualified Regex.Internal.Parser as P
import Regex.Internal.Regex (RE(..), Greediness(..), Strictness(..))
import qualified Regex.Internal.Regex as R
import qualified Regex.Internal.Num as RNum
import qualified Regex.Internal.Generated.CaseFold as CF

----------------------
-- Token and Text REs
----------------------

-- | The token type used for parsing @Text@.

-- This module uses RE TextToken for Text regexes instead of simply RE Char to
-- support Text slicing. It does mean that use cases not using slicing pay a
-- small cost, but it is not worth having two separate Text regex APIs.
--
-- Slicing is made possible by the unsafeAdjacentAppend function. Of course,
-- this means that REs using it MUST NOT be used with multiple Texts, such as
-- trying to parse chunks of a lazy Text.
data TextToken = TextToken
  { tArr     :: {-# UNPACK #-} !TArray.Array
  , tOffset  :: {-# UNPACK #-} !Int
  , tChar    :: {-# UNPACK #-} !Char
  }

-- | A type alias for convenience.
--
-- A function which accepts a @RE c a@ will accept a @REText a@.
type REText = RE TextToken

-- | A type alias for convenience.
--
-- A function which accepts a @Parser c a@ will accept a @ParserText a@.
type ParserText = Parser TextToken

-- | Parse a @Char@ into an @a@ if the given function returns @Just@.
token :: (Char -> Maybe a) -> REText a
token t = R.token (\ !tok -> t (tChar tok))
{-# INLINE token #-}

-- | Parse a @Char@ if it satisfies the given predicate.
satisfy :: (Char -> Bool) -> REText Char
satisfy p = token $ \c -> if p c then Just c else Nothing
{-# INLINE satisfy #-}

-- | Parse the given @Char@.
char :: Char -> REText Char
char !c = satisfy (c==)

-- | Parse the given @Char@, ignoring case.
--
-- Comparisons are performed after applying
-- [simple case folding](https://www.unicode.org/reports/tr44/#Simple_Case_Folding)
-- as described by the Unicode standard.
charIgnoreCase :: Char -> REText Char
charIgnoreCase c = satisfy $ (c'==) . CF.caseFoldSimple
  where
    !c' = CF.caseFoldSimple c
-- See Note [Why simple case fold]

-- | Parse any @Char@.
anyChar :: REText Char
anyChar = token Just

-- | Parse a @Char@ if it is a member of the @CharSet@.
oneOf :: CharSet -> REText Char
oneOf !cs = satisfy (`CS.member` cs)

-- | Parse the given @Text@.
text :: Text -> REText Text
text t = t <$ T.foldr' ((*>) . char) (pure ()) t

-- | Parse the given @Text@, ignoring case.
--
-- Comparisons are performed after applying
-- [simple case folding](https://www.unicode.org/reports/tr44/#Simple_Case_Folding)
-- as described by the Unicode standard.
textIgnoreCase :: Text -> REText Text
textIgnoreCase t =
  T.foldr' (\c cs -> R.liftA2' unsafeAdjacentAppend (ignoreCaseTokenMatch c) cs)
           (pure T.empty)
           t
-- See Note [Why simple case fold]

-- | Parse any @Text@. Biased towards matching more.
manyText :: REText Text
manyText = R.foldlMany' unsafeAdjacentAppend T.empty anyTokenMatch

-- | Parse any non-empty @Text@. Biased towards matching more.
someText :: REText Text
someText = R.liftA2' unsafeAdjacentAppend anyTokenMatch manyText

-- | Parse any @Text@. Minimal, i.e. biased towards matching less.
manyTextMin :: REText Text
manyTextMin = R.foldlManyMin' unsafeAdjacentAppend T.empty anyTokenMatch

-- | Parse any non-empty @Text@. Minimal, i.e. biased towards matching less.
someTextMin :: REText Text
someTextMin = R.liftA2' unsafeAdjacentAppend anyTokenMatch manyTextMin

-- | Parse any @Text@ containing members of the @CharSet@.
-- Biased towards matching more.
manyTextOf :: CharSet -> REText Text
manyTextOf !cs = R.foldlMany' unsafeAdjacentAppend T.empty (oneOfTokenMatch cs)

-- | Parse any non-empty @Text@ containing members of the @CharSet@.
-- Biased towards matching more.
someTextOf :: CharSet -> REText Text
someTextOf !cs = R.liftA2' unsafeAdjacentAppend (oneOfTokenMatch cs) (manyTextOf cs)

-- | Parse any @Text@ containing members of the @CharSet@.
-- Minimal, i.e. biased towards matching less.
manyTextOfMin :: CharSet -> REText Text
manyTextOfMin !cs = R.foldlManyMin' unsafeAdjacentAppend T.empty (oneOfTokenMatch cs)

-- | Parse any non-empty @Text@ containing members of the @CharSet@.
-- Minimal, i.e. biased towards matching less.
someTextOfMin :: CharSet -> REText Text
someTextOfMin !cs =
  R.liftA2' unsafeAdjacentAppend (oneOfTokenMatch cs) (manyTextOfMin cs)

-----------------
-- Numeric REs
-----------------

-- | Parse a decimal @Natural@.
-- Leading zeros are not accepted. Biased towards matching more.
naturalDec :: REText Natural
naturalDec = RNum.mkNaturalDec digitRange

-- | Parse a decimal @Integer@. Parse an optional sign, @\'-\'@ or @\'+\'@,
-- followed by the given @RE@, followed by the absolute value of the integer.
-- Leading zeros are not accepted. Biased towards matching more.
integerDec :: REText a -> REText Integer
integerDec sep = RNum.mkSignedInteger minus plus (sep *> naturalDec)

-- | Parse a hexadecimal @Natural@. Both uppercase @\'A\'..\'F\'@ and lowercase
-- @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
naturalHex :: REText Natural
naturalHex = RNum.mkNaturalHex hexDigitRange

-- | Parse a hexadecimal @Integer@. Parse an optional sign, @\'-\'@ or @\'+\'@,
-- followed by the given @RE@, followed by the absolute value of the integer.
-- Both uppercase @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
integerHex :: REText a -> REText Integer
integerHex sep = RNum.mkSignedInteger minus plus (sep *> naturalHex)

-- | Parse a decimal @Word@ in the range @[low..high]@.
-- Leading zeros are not accepted. Biased towards matching more.
wordRangeDec :: (Word, Word) -> REText Word
wordRangeDec lh = RNum.mkWordRangeDec digitRange lh

-- | Parse a decimal @Int@ in the range @[low..high]@. Parse an optional sign,
-- @\'-\'@ or @\'+\'@, followed by the given @RE@, followed by the absolute
-- value of the integer.
-- Leading zeros are not accepted. Biased towards matching more.
intRangeDec :: REText a -> (Int, Int) -> REText Int
intRangeDec sep lh =
  RNum.mkSignedIntRange minus plus ((sep *>) . wordRangeDec) lh

-- | Parse a hexadecimal @Word@ in the range @[low..high]@. Both uppercase
-- @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
wordRangeHex :: (Word, Word) -> REText Word
wordRangeHex lh = RNum.mkWordRangeHex hexDigitRange lh

-- | Parse a hexadecimal @Int@ in the range @[low..high]@. Parse an optional
-- sign, @\'-\'@ or @\'+\'@, followed by the given @RE@, followed by the
-- absolute value of the integer.
-- Both uppercase @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
intRangeHex :: REText a -> (Int, Int) -> REText Int
intRangeHex sep lh =
  RNum.mkSignedIntRange minus plus ((sep *>) . wordRangeHex) lh

-- | Parse a @Word@ of exactly n decimal digits, including any leading zeros.
-- Will not parse values that do not fit in a @Word@.
-- Biased towards matching more.
wordDecN :: Int -> REText Word
wordDecN n = RNum.mkWordDecN digitRange n

-- | Parse a @Word@ of exactly n hexadecimal digits, including any leading
-- zeros. Both uppercase @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are
-- accepted. Will not parse values that do not fit in a @Word@.
-- Biased towards matching more.
wordHexN :: Int -> REText Word
wordHexN n = RNum.mkWordHexN hexDigitRange n

minus, plus :: REText ()
minus = token $ \c -> if c == '-' then Just () else Nothing
plus = token $ \c -> if c == '+' then Just () else Nothing

-- l and h must be in [0..9]
digitRange :: Word -> Word -> REText Word
digitRange !l !h = token $ \c ->
  let d = fromIntegral (ord c - ord '0')
  in if l <= d && d <= h then Just d else Nothing

-- l and h must be in [0..15]
hexDigitRange :: Word -> Word -> REText Word
hexDigitRange !l !h = token $ \c ->
  let dec = fromIntegral (ord c - ord '0')
      hexl = fromIntegral (ord c - ord 'a')
      hexu = fromIntegral (ord c - ord 'A')
  in do
    d <- case () of
      _ | dec <= 9 -> Just dec
        | hexl <= 5 -> Just $! 10 + hexl
        | hexu <= 5 -> Just $! 10 + hexu
        | otherwise -> Nothing
    if l <= d && d <= h then Just d else Nothing
-- TODO: This can surely be optimized

----------------
-- Match stuff
----------------

tokenToSlice :: TextToken -> Text
tokenToSlice t =
  TInternal.Text (tArr t) (tOffset t) (TInternalUtf8.utf8Length (tChar t))

tokenMatch :: (TextToken -> Maybe a) -> REText Text
tokenMatch t = R.token (\ !tok -> tokenToSlice tok <$ t tok)

tokenWithMatch :: (TextToken -> Maybe a) -> REText (WithMatch a)
tokenWithMatch t = R.token (\ !tok -> WM (tokenToSlice tok) <$> t tok)

anyTokenMatch :: REText Text
anyTokenMatch = R.token (\tok -> Just $! tokenToSlice tok)

ignoreCaseTokenMatch :: Char -> REText Text
ignoreCaseTokenMatch c = R.token $ \tok ->
  if CF.caseFoldSimple (tChar tok) == c'
  then Just $! tokenToSlice tok
  else Nothing
  where
    !c' = CF.caseFoldSimple c

oneOfTokenMatch :: CharSet -> REText Text
oneOfTokenMatch !cs = R.token $ \tok ->
  if CS.member (tChar tok) cs
  then Just $! tokenToSlice tok
  else Nothing

-- | Rebuild the @RE@ such that the result is the matched @Text@ instead.
toMatch :: REText a -> REText Text
toMatch = go
  where
    go :: REText b -> REText Text
    go re = case re of
      RToken t -> tokenMatch t
      RFmap _ _ re1 -> go re1
      RFmap_ _ re1 -> go re1
      RPure _ -> RPure T.empty
      RLiftA2 _ _ re1 re2 ->
        RLiftA2 Strict unsafeAdjacentAppend (go re1) (go re2)
      REmpty -> REmpty
      RAlt re1 re2 -> RAlt (go re1) (go re2)
      RMany _ _ _ _ re1 ->
        RFold Strict Greedy unsafeAdjacentAppend T.empty (go re1)
      RFold _ gr _ _ re1 ->
        RFold Strict gr unsafeAdjacentAppend T.empty (go re1)

data WithMatch a = WM {-# UNPACK #-} !Text a

instance Functor WithMatch where
  fmap f (WM t x) = WM t (f x)

fmapWM' :: (a -> b) -> WithMatch a -> WithMatch b
fmapWM' f (WM t x) = WM t $! f x

instance Applicative WithMatch where
  pure = WM T.empty
  liftA2 f (WM t1 x) (WM t2 y) = WM (unsafeAdjacentAppend t1 t2) (f x y)

liftA2WM' :: (a1 -> a2 -> b) -> WithMatch a1 -> WithMatch a2 -> WithMatch b
liftA2WM' f (WM t1 x) (WM t2 y) = WM (unsafeAdjacentAppend t1 t2) $! f x y

-- | Rebuild the @RE@ to include the matched @Text@ alongside the result.
withMatch :: REText a -> REText (Text, a)
withMatch = R.fmap' (\(WM t x) -> (t,x)) . go
  where
    go :: REText b -> REText (WithMatch b)
    go re = case re of
      RToken t -> tokenWithMatch t
      RFmap st f re1 ->
        let g = case st of
              Strict -> fmapWM' f
              NonStrict -> fmap f
        in RFmap Strict g (go re1)
      RFmap_ b re1 -> RFmap Strict (flip WM b) (toMatch re1)
      RPure b -> RPure (pure b)
      RLiftA2 st f re1 re2 ->
        let g = case st of
              Strict -> liftA2WM' f
              NonStrict -> liftA2 f
        in RLiftA2 Strict g (go re1) (go re2)
      REmpty -> REmpty
      RAlt re1 re2 -> RAlt (go re1) (go re2)
      RMany f1 f2 f z re1 ->
        RMany (fmapWM' f1) (fmapWM' f2) (liftA2WM' f) (pure z) (go re1)
      RFold st gr f z re1 ->
        let g = case st of
              Strict -> liftA2WM' f
              NonStrict -> liftA2 f
        in RFold Strict gr g (pure z) (go re1)

----------
-- Parse
----------

textTokenFoldr :: (TextToken -> b -> b) -> b -> Text -> b
textTokenFoldr f z (TInternal.Text a o0 l) = loop o0
  where
    loop o | o - o0 >= l = z
    loop o = case TUnsafe.iterArray a o of
      TUnsafe.Iter c clen -> f (TextToken a o c) (loop (o + clen))
{-# INLINE textTokenFoldr #-}

-- | \(O(mn \log m)\). Parse a @Text@ with a @REText@.
--
-- Parses the entire @Text@, not just a prefix or a substring.
--
-- Uses 'Regex.Text.compile', see the note there.
--
-- If parsing multiple @Text@s using the same @RE@, it is wasteful to compile
-- the @RE@ every time. So, prefer to
--
-- * Compile once with 'Regex.Text.compile' or 'Regex.Text.compileBounded' and
--   use the compiled 'ParserText'  with 'parse' as many times as required.
-- * Alternately, partially apply this function to a @RE@ and use the function
--   as many times as required.
reParse :: REText a -> Text -> Maybe a
reParse re = let !p = P.compile re in parse p
{-# INLINE reParse #-}

-- | \(O(mn \log m)\). Parse a @Text@ with a @ParserText@.
--
-- Parses the entire @Text@, not just a prefix or a substring.
parse :: ParserText a -> Text -> Maybe a
parse = P.parseFoldr textTokenFoldr

-- | \(O(mn \log m)\). Parse a @Text@ with a @ParserText@. Calls 'error' on
-- parse failure.
--
-- For use with parsers that are known to never fail.
--
-- Parses the entire @Text@, not just a prefix or a substring.
parseSure :: ParserText a -> Text -> a
parseSure p = fromMaybe parseSureError . parse p

parseSureError :: a
parseSureError = errorWithoutStackTrace
  "Regex.Text.parseSure: parse failed; if parsing can fail use 'parse' instead"

reParseSure :: REText a -> Text -> a
reParseSure re = fromMaybe parseSureError . reParse re
{-# INLINE reParseSure #-}

-- | \(O(mn \log m)\). Find the first occurence of the given @RE@ in a @Text@.
--
-- ==== __Examples__
--
-- >>> find (text "meow") "homeowner"
-- Just "meow"
--
-- To test whether a @Text@ is present in another @Text@, like above, prefer
-- @Data.Text.'T.isInfixOf'@.
--
-- >>> find (textIgnoreCase "haskell") "Look I'm Haskelling!"
-- Just "Haskell"
-- >>> find (text "backtracking") "parser-regex"
-- Nothing
--
find :: REText a -> Text -> Maybe a
find = reParse . R.toFind
{-# INLINE find #-}

-- | \(O(mn \log m)\). Find all non-overlapping occurences of the given @RE@ in
-- the @Text@.
--
-- ==== __Examples__
--
-- >>> findAll (text "ana") "banananana"
-- ["ana","ana"]
--
-- @
-- data Roll = Roll
--   Natural -- ^ Rolls
--   Natural -- ^ Faces on the die
--   deriving Show
--
-- roll :: REText Roll
-- roll = Roll \<$> ('naturalDec' \<|> pure 1) \<* 'char' \'d\' \<*> naturalDec
-- @
--
-- >>> findAll roll "3d6, d10, 2d10"
-- [Roll 3 6,Roll 1 10,Roll 2 10]
--
findAll :: REText a -> Text -> [a]
findAll = reParseSure . R.toFindMany
{-# INLINE findAll #-}

-- | \(O(mn \log m)\). Split a @Text@ at occurences of the given @RE@.
--
-- ==== __Examples__
--
-- >>> splitOn (char ' ') "Glasses are really versatile"
-- ["Glasses","are","really","versatile"]
--
-- For simple splitting, like above, prefer @Data.Text.'Data.Text.words'@,
-- @Data.Text.'Data.Text.lines'@, @Data.Text.'Data.Text.split'@ or
-- @Data.Text.'Data.Text.splitOn'@, whichever is applicable.
--
-- >>> splitOn (char ' ' *> oneOf "+-=" *> char ' ') "3 - 1 + 1/2 - 2 = 0"
-- ["3","1","1/2","2","0"]
--
-- If the @Text@ starts or ends with a delimiter, the result will contain
-- empty @Text@s at those positions.
--
-- >>> splitOn (char 'a') "ayaya"
-- ["","y","y",""]
--
splitOn :: REText a -> Text -> [Text]
splitOn = reParseSure . toSplitOn
{-# INLINE splitOn #-}

toSplitOn :: REText a -> REText [Text]
toSplitOn re = manyTextMin `R.sepBy` re

-- | \(O(mn \log m)\). Replace the first match of the given @RE@ with its
-- result. If there is no match, the result is @Nothing@.
--
-- ==== __Examples__
--
-- >>> replace ("world" <$ text "Haskell") "Hello, Haskell!"
-- Just "Hello, world!"
--
-- >>> replace ("," <$ some (char '.')) "one...two...ten"
-- Just "one,two...ten"
--
replace :: REText Text -> Text -> Maybe Text
replace = reParse . toReplace
{-# INLINE replace #-}

toReplace :: REText Text -> REText Text
toReplace re = liftA2 f manyTextMin re <*> manyText
  where
    f a b c = reverseConcat [c,b,a]

-- | \(O(mn \log m)\). Replace all non-overlapping matches of the given @RE@
-- with their results.
--
-- ==== __Examples__
--
-- >>> replaceAll (" and " <$ text ", ") "red, blue, green"
-- "red and blue and green"
--
-- For simple replacements like above, prefer @Data.Text.'Data.Text.replace'@.
--
-- >>> replaceAll ("Fruit" <$ text "Time" <|> "a banana" <$ text "an arrow") "Time flies like an arrow"
-- "Fruit flies like a banana"
--
-- @
-- sep = 'oneOf' "-./"
-- digits n = 'toMatch' ('Control.Monad.replicateM_' n (oneOf 'Data.CharSet.digit'))
-- toYmd d m y = mconcat [y, \"-\", m, \"-\", d]
-- date = toYmd \<$> digits 2 \<* sep
--              \<*> digits 2 \<* sep
--              \<*> digits 4
-- @
-- >>> replaceAll date "01/01/1970, 01-04-1990, 03.07.2011"
-- "1970-01-01, 1990-04-01, 2011-07-03"
--
replaceAll :: REText Text -> Text -> Text
replaceAll = reParseSure . toReplaceMany
{-# INLINE replaceAll #-}

toReplaceMany :: REText Text -> REText Text
toReplaceMany re =
  reverseConcat <$> R.foldlMany' (flip (:)) [] (re <|> anyTokenMatch)

-------------------------
-- Low level Text stuff
-------------------------

-- WARNING: If t1 and t2 are not empty, they must be adjacent slices of the
-- same Text. In other words, sameByteArray# a1 _a2 && o1 + l1 == _o2.
unsafeAdjacentAppend :: Text -> Text -> Text
unsafeAdjacentAppend t1@(TInternal.Text a1 o1 l1) t2@(TInternal.Text _a2 _o2 l2)
  | T.null t1 = t2
  | T.null t2 = t1
  | otherwise = TInternal.Text a1 o1 (l1+l2)

-- reverseConcat = T.concat . reverse
reverseConcat :: [Text] -> Text
reverseConcat ts = case ts of
  [] -> T.empty
  [t] -> t
  _ | len == 0 -> T.empty
    | otherwise -> TInternal.Text arr 0 len
  where
    flen acc (TInternal.Text _ _ l)
      | acc' >= 0 = acc'
      | otherwise = reverseConcatOverflowError
      where
        acc' = acc + l
    len = F.foldl' flen 0 ts
    arr = TArray.run $ do
      marr <- TArray.new len
      let loop !_ [] = pure marr
          loop i (TInternal.Text a o l : ts') =
            TArray.copyI l marr (i-l) a o *> loop (i-l) ts'
      loop len ts

reverseConcatOverflowError :: a
reverseConcatOverflowError =
  errorWithoutStackTrace "Regex.Text.reverseConcat: size overflow"

----------
-- Notes
----------

-- Note [Why simple case fold]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Unicode defines two different ways to case fold, "simple" and "full". Full is
-- superior to simple, and capable of folding more pairs of texts to the same
-- text. This is what is used by Data.Text.toCaseFold.
--
-- However, full maps a Char to one or more Chars, for instance "ß" maps to
-- "ss". Since we operate on one Char at a time without backtracking, we must
-- have branching in our regex corresponding to possible texts that case fold to
-- a target text. For instance, to match "sssss" with full case fold given the
-- above mapping, possible inputs are
--
-- sssss, sssß, ssßs, sßss, ßsss, sßß, ßsß, ßßs
--
-- Fun fact: the number of strings that match "s"*n is Fibonacci(n+1).
-- Of course, we can't have textIgnoreCase take a text and explode into a regex
-- of exponential size.
--
-- So, we restrict ourselves to simple case folding. Simple case folding
-- maps a single Char to a single Char. And it's easy to test that the required
-- Char and a Char in the input case fold to the same Char.
--
-- Note that charIgnoreCase could possibly use full case folding. Only a small
-- number of texts would case fold to the case fold of a single Char. But we
-- stick with simple case fold for consistency.
