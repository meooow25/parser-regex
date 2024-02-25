{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Regex.Internal.Regex
  ( RE(..)
  , Strictness(..)
  , Greediness(..)
  , Many(..)

  , token
  , anySingle
  , single
  , satisfy
  , foldable

  , foldlMany
  , foldlManyMin
  , manyr
  , optionalMin
  , someMin
  , manyMin
  , atLeast
  , atMost
  , betweenCount
  , atLeastMin
  , atMostMin
  , betweenCountMin
  , sepBy
  , sepBy1
  , endBy
  , endBy1
  , sepEndBy
  , sepEndBy1
  , chainl1
  , chainr1
  , toFind
  , toFindMany

  , fmap'
  , liftA2'
  , foldlMany'
  , foldlManyMin'
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(..), NFData1(..), rnf1)
import Control.Monad
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import qualified Data.Foldable as F

---------------------------------
-- RE and constructor functions
---------------------------------

-- | A regular expression. Operates on a sequence of elements of type @c@ and
-- capable of parsing into an @a@.
--
-- A @RE@ is a Functor, Applicative, and Alternative.
--
-- * 'pure': Succeed without consuming input.
-- * 'liftA2', '<*>', '*>', '<*': Sequential composition.
-- * 'empty': Fail.
-- * '<|>': Alternative composition. Left-biased, i.e. the result of parsing
--   using @a \<|> b@ is the result of parsing using @a@ if it succeeds,
--   otherwise it is the result of parsing using @b@ if it succeeds,
--   otherwise parsing fails.
-- * 'many': Zero or more. @many a@ parses multiple @a@s sequentially. Biased
--   towards matching more. Use 'manyMin' for a bias towards matching less.
--   Also see the section "Looping parsers".
-- * 'some': One or more. @some a@ parses multiple @a@s sequentially. Biased
--   towards matching more. Use 'someMin' for a bias towards matching less.
--
-- In addition to expected Functor, Applicative, and Alternative laws,
-- @RE@ obeys these Applicative-Alternative laws:
--
-- @
-- a \<*> empty = empty
-- empty \<*> a = empty
-- (a \<|> b) \<*> c = (a \<*> c) \<|> (b \<*> c)
-- a \<*> (b \<|> c) = (a \<*> b) \<|> (a \<*> c)
-- @
--
-- Note that, because of bias, it is /not true/ that @a \<|> b = b \<|> a@.
--
-- /Performance note/: Prefer the smaller of equivalent regexes, i.e. prefer
-- @(a \<|> b) \<*> c@ over @(a \<*> c) \<|> (b \<*> c)@.
--
data RE c a where
  RToken  :: !(c -> Maybe a) -> RE c a
  RFmap   :: !Strictness -> !(a1 -> a) -> !(RE c a1) -> RE c a
  RFmap_  :: a -> !(RE c a1) -> RE c a
  RPure   :: a -> RE c a
  RLiftA2 :: !Strictness -> !(a1 -> a2 -> a) -> !(RE c a1) -> !(RE c a2) -> RE c a
  REmpty  :: RE c a
  RAlt    :: !(RE c a) -> !(RE c a) -> (RE c a)
  RFold   :: !Strictness -> !Greediness -> !(a -> a1 -> a) -> a -> !(RE c a1) -> RE c a
  RMany   :: !(a1 -> a) -> !(a2 -> a) -> !(a2 -> a1 -> a2) -> !a2 -> !(RE c a1) -> RE c a -- Strict and greedy implicitly

data Strictness = Strict | NonStrict
data Greediness = Greedy | Minimal

instance Functor (RE c) where
  fmap = RFmap NonStrict
  (<$) = RFmap_

fmap' :: (a -> b) -> RE c a -> RE c b
fmap' = RFmap Strict

instance Applicative (RE c) where
  pure = RPure
  liftA2 = RLiftA2 NonStrict
  re1 *> re2 = liftA2 (const id) (void re1) re2
  re1 <* re2 = liftA2 const re1 (void re2)

liftA2' :: (a1 -> a2 -> b) -> RE c a1 -> RE c a2 -> RE c b
liftA2' = RLiftA2 Strict

instance Alternative (RE c) where
  empty = REmpty
  (<|>) = RAlt
  some re = liftA2' (:) re (many re)
  many = fmap reverse . foldlMany' (flip (:)) []

-- | Parse a @c@ into an @a@ if the given function returns @Just@.
token :: (c -> Maybe a) -> RE c a
token = RToken

-- | Zero or more. Biased towards matching more.
--
-- Also see the section "Looping parsers".
manyr :: RE c a -> RE c (Many a)
manyr = RMany Repeat (Finite . reverse) (flip (:)) []

-- | Parse many occurences of the given @RE@. Biased towards matching more.
--
-- Also see the section "Looping parsers".
foldlMany :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlMany = RFold NonStrict Greedy

foldlMany' :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlMany' f !z = RFold Strict Greedy f z

-- | Parse many occurences of the given @RE@. Minimal, i.e. biased towards
-- matching less.
foldlManyMin :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlManyMin = RFold NonStrict Minimal

foldlManyMin' :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlManyMin' f !z = RFold Strict Minimal f z

-- | Parse a @c@ if it satisfies the given predicate.
satisfy :: (c -> Bool) -> RE c c
satisfy p = token (\c -> if p c then Just c else Nothing)
{-# INLINE satisfy #-}

-- | Parse the given @c@.
single :: Eq c => c -> RE c c
single !c = satisfy (c==)

-- | Parse any @c@.
anySingle :: RE c c
anySingle = token Just

-- | Parse the given chunk of @c@s.
foldable :: (Foldable f, Eq c) => f c -> RE c (f c)
foldable xs = xs <$ foldr ((*>) . single) (pure ()) xs

---------
-- Many
---------

data Many a
  = Repeat a   -- ^ A single value repeating indefinitely
  | Finite [a] -- ^ A finite list
  deriving (Eq, Show)

instance Ord a => Ord (Many a) where
  compare (Repeat x) (Repeat y) = compare x y
  compare xs ys = compare (F.toList xs) (F.toList ys)

instance Eq1 Many where
  liftEq f m1 m2 = case (m1,m2) of
    (Repeat x, Repeat y) -> f x y
    (Finite xs, Finite ys) -> liftEq f xs ys
    _ -> False

instance Ord1 Many where
  liftCompare f m1 m2 = case (m1,m2) of
    (Repeat x, Repeat y) -> f x y
    _ -> liftCompare f (F.toList m1) (F.toList m2)

instance Show1 Many where
  liftShowsPrec sp sl p m = case m of
    Repeat x -> showsUnaryWith sp "Repeat" p x
    Finite xs -> showParen (p > 10) $
      showString "Finite" . showChar ' ' . sl xs

instance Functor Many where
  fmap f m = case m of
    Repeat x -> Repeat (f x)
    Finite xs -> Finite (map f xs)

instance Foldable Many where
  foldr f z m = case m of
    Repeat x -> let r = f x r in r
    Finite xs -> foldr f z xs

  foldl' f z m = case m of
    Repeat _ -> error "Foldable Many: Repeat: foldl'"
    Finite xs -> F.foldl' f z xs

  foldl f z m = case m of
    Repeat _ -> error "Foldable Many: Repeat: foldl"
    Finite xs -> foldl f z xs

  toList m = case m of
    Repeat x -> repeat x
    Finite xs -> xs

instance NFData a => NFData (Many a) where
  rnf = rnf1

instance NFData1 Many where
  liftRnf f m = case m of
    Repeat x -> f x
    Finite xs -> liftRnf f xs

----------------
-- Combinators
----------------

-- | Zero or one. Minimal, i.e. biased towards zero.
--
-- @Use Control.Applicative.'optional'@ for the same but biased towards one.
optionalMin :: RE c a -> RE c (Maybe a)
optionalMin re = pure Nothing <|> Just <$> re

-- | One or more. Minimal, i.e. biased towards matching less.
someMin :: RE c a -> RE c [a]
someMin re = liftA2' (:) re (manyMin re)

-- | Zero or more. Minimal, i.e. biased towards matching less.
manyMin :: RE c a -> RE c [a]
manyMin = fmap reverse . foldlManyMin' (flip (:)) []

-- | At least n times. Biased towards matching more.
atLeast :: Int -> RE c a -> RE c [a]
atLeast n re = replicateAppendM (max n 0) re (many re)

-- | At most n times. Biased towards matching more.
atMost :: Int -> RE c a -> RE c [a]
atMost n = betweenCount (0,n)

-- | Between m and n times (inclusive). Biased towards matching more.
betweenCount :: (Int, Int) -> RE c a -> RE c [a]
betweenCount (l,h) re
  | l' > h = empty
  | otherwise = replicateAppendM l' re (go (h - l'))
  where
    l' = max l 0
    go 0 = pure []
    go n = liftA2' (:) re (go (n-1)) <|> pure []

-- | At least n times. Minimal, i.e. biased towards matching less.
atLeastMin :: Int -> RE c a -> RE c [a]
atLeastMin n re = replicateAppendM (max n 0) re (manyMin re)

-- | At most n times. Minimal, i.e. biased towards matching less.
atMostMin :: Int -> RE c a -> RE c [a]
atMostMin n = betweenCountMin (0,n)

-- | Between m and n times (inclusive). Minimal, i.e. biased towards matching
-- less.
betweenCountMin :: (Int, Int) -> RE c a -> RE c [a]
betweenCountMin (l,h) re
  | l' > h = empty
  | otherwise = replicateAppendM l' re (go (h - l'))
  where
    l' = max l 0
    go 0 = pure []
    go n = pure [] <|> liftA2' (:) re (go (n-1))

-- n0 must be >= 0
replicateAppendM :: Int -> RE c a -> RE c [a] -> RE c [a]
replicateAppendM n0 re re1 = go n0
  where
    go 0 = re1
    go n = liftA2' (:) re (go (n-1))

-- | @r \`sepBy\` sep@ parses zero or more occurences of @r@, separated by
-- @sep@. Biased towards matching more.
sepBy :: RE c a -> RE c sep -> RE c [a]
sepBy re sep = sepBy1 re sep <|> pure []

-- | @r \`sepBy1\` sep@ parses one or more occurences of @r@, separated by
-- @sep@. Biased towards matching more.
sepBy1 :: RE c a -> RE c sep -> RE c [a]
sepBy1 re sep = liftA2' (:) re (many (sep *> re))

-- | @r \`endBy\` sep@ parses zero or more occurences of @r@, separated and
-- ended by @sep@. Biased towards matching more.
endBy :: RE c a -> RE c sep -> RE c [a]
endBy re sep = many (re <* sep)

-- | @r \`endBy1\` sep@ parses one or more occurences of @r@, separated and
-- ended by @sep@. Biased towards matching more.
endBy1 :: RE c a -> RE c sep -> RE c [a]
endBy1 re sep = some (re <* sep)

-- | @r \`sepEndBy\` sep@ parses zero or more occurences of @r@, separated and
-- optionally ended by @sep@. Biased towards matching more.
sepEndBy :: RE c a -> RE c sep -> RE c [a]
sepEndBy re sep = sepEndBy1 re sep <|> pure []

-- | @r \`sepEndBy1\` sep@ parses one or more occurences of @r@, separated and
-- optionally ended by @sep@. Biased towards matching more.
sepEndBy1 :: RE c a -> RE c sep -> RE c [a]
sepEndBy1 re sep = sepBy1 re sep <* optional sep

-- | @chainl1 r op@ parses one or more occurences of @r@, separated by @op@.
-- The result is obtained by left associative application of all functions
-- returned by @op@ to the values returned by @p@. Biased towards matching more.
chainl1 :: RE c a -> RE c (a -> a -> a) -> RE c a
chainl1 re op = liftA2 (flip id) re rest
  where
    rest = foldlMany (flip (.)) id (liftA2 flip op re)

-- | @chainr1 r op@ parses one or more occurences of @r@, separated by @op@.
-- The result is obtained by right associative application of all functions
-- returned by @op@ to the values returned by @p@. Biased towards matching more.
chainr1 :: RE c a -> RE c (a -> a -> a) -> RE c a
chainr1 re op = liftA2 id rest re
  where
    rest = foldlMany (.) id (liftA2 (flip id) re op)

-- | Results in the first occurence of the given @RE@. Fails if no occurence
-- is found.
toFind :: RE c a -> RE c a
toFind re = manyMin anySingle *> re <* many anySingle

-- | Results in all non-overlapping occurences of the given @RE@. Always
-- succeeds.
toFindMany :: RE c a -> RE c [a]
toFindMany re =
  reverse <$>
  foldlMany' (flip ($)) [] ((:) <$> re <|> id <$ anySingle)
