-- | This module exports base types and functions. You can use these to define
-- functions to work on arbitrary sequence types. If you want to work with
-- @Text@ or @String@, import and use "Regex.Text" or "Regex.List" instead.
module Regex.Base
  (
    -- * @RE@ and @Parser@
    R.RE
  , P.Parser

    -- * Compile
  , P.compile
  , P.compileBounded

    -- * Parse
    -- $parse
  , P.ParserState
  , P.prepareParser
  , P.stepParser
  , P.finishParser
  , P.Foldr
  , P.parseFoldr

    -- * @RE@s and combinators
  , R.token
  , R.anySingle
  , R.single
  , R.satisfy
  , R.foldable
  , R.foldlMany
  , R.foldlManyMin
  , R.Many(..)
  , R.manyr
  , R.optionalMin
  , R.someMin
  , R.manyMin
  , R.atLeast
  , R.atMost
  , R.betweenCount
  , R.atLeastMin
  , R.atMostMin
  , R.betweenCountMin
  , R.sepBy
  , R.sepBy1
  , R.endBy
  , R.endBy1
  , R.sepEndBy
  , R.sepEndBy1
  , R.chainl1
  , R.chainr1
  , R.toFind
  , R.toFindMany

    -- * Strict combinators
    -- $strict

  , R.fmap'
  , R.liftA2'
  , R.foldlMany'
  , R.foldlManyMin'
  ) where

import qualified Regex.Internal.Regex as R
import qualified Regex.Internal.Parser as P

-- $parse
--
-- The functions @prepareParser@, @stepParser@, and @finishParser@ grant
-- a large amount of control over the parsing process, making it possible to
-- parse monadically, or in a branching manner, or even both.
--
-- As a simpler alternative to the trio of functions above, @parseFoldr@ can be
-- used on any sequence type that can be folded over.
--

-- $strict
--
-- These combinators force the result before continuing parsing. But beware!
-- If that particular parse ends up failing, the work done will have been for
-- nothing. This can blow up the complexity of parsing. For instance,
-- @fmap' sum (many digit)@ is \(O(n^2)\).
--
-- These functions are intended to be used when the work done in forcing the
-- result is guaranteed to be cheaper than creating a thunk, saving memory and
-- time.
-- For instance, @liftA2' (:)@ is a good usage, since @(:)@ does a small amount
-- of work and a thunk is avoided. As another example, @liftA2' ((+) \@Int)@ is
-- /not/ a good usage, because @(+)@ is strict and forces its arguments,
-- performing an arbitrary amount of work. However, it is okay to use
-- @liftA2' ((+) \@Int)@ if it is known for certain that its arguments will be
-- in WHNF.
--
-- __WARNING__: If you are not sure whether to use these function,
-- /don't use these functions/. Simply use @fmap@, @liftA2@, @foldlMany@ or
-- @foldlManyMin@ instead.
--
