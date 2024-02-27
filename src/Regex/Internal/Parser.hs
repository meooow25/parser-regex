{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regex.Internal.Parser
  ( Parser(..)
  , Node(..)
  , compile
  , compileBounded

  , ParserState
  , prepareParser
  , stepParser
  , finishParser
  , Foldr
  , parseFoldr
  , stepParserK
  , FoldrK
  , parseFoldrK
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Fix
import Data.Maybe (isJust)
import qualified Data.Foldable as F
import GHC.Exts (TYPE)

import Regex.Internal.Regex (RE(..), Strictness(..), Greediness(..))
import Regex.Internal.Unique (Unique(..), UniqueSet)
import qualified Regex.Internal.Unique as U

----------
-- Types
----------

-- | A parser compiled from a @'RE' c a@.
data Parser (c :: TYPE crep) a where
  PToken  :: forall {crep} (c :: TYPE crep) a.
             !(c -> Maybe a) -> Parser c a
  PFmap   :: forall {crep} (c :: TYPE crep) a a1.
             !Strictness -> !(a1 -> a) -> !(Parser c a1) -> Parser c a
  PFmap_  :: forall {crep} (c :: TYPE crep) a.
             !(Node c a) -> Parser c a
  PPure   :: forall {crep} (c :: TYPE crep) a.
             a -> Parser c a
  PLiftA2 :: forall {crep} (c :: TYPE crep) a a1 a2.
             !Strictness -> !(a1 -> a2 -> a) -> !(Parser c a1) -> !(Parser c a2) -> Parser c a
  PEmpty  :: forall {crep} (c :: TYPE crep) a.
             Parser c a
  PAlt    :: forall {crep} (c :: TYPE crep) a.
             !Unique -> !(Parser c a) -> !(Parser c a) -> Parser c a
  PFoldGr :: forall {crep} (c :: TYPE crep) a a1.
             !Unique -> !Strictness -> !(a -> a1 -> a) -> a -> !(Parser c a1) -> Parser c a
  PFoldMn :: forall {crep} (c :: TYPE crep) a a1.
             !Unique -> !Strictness -> !(a -> a1 -> a) -> a -> !(Parser c a1) -> Parser c a
  PMany   :: forall {crep} (c :: TYPE crep) a a1 a2.
             !Unique -> !(a1 -> a) -> !(a2 -> a) -> !(a2 -> a1 -> a2) -> !a2 -> !(Parser c a1) -> Parser c a

-- | A node in the NFA. Used for recognition.
data Node (c :: TYPE crep) a where
  NAccept :: forall {crep} (c :: TYPE crep) a.
             a -> Node c a
  NGuard  :: forall {crep} (c :: TYPE crep) a.
             !Unique -> Node c a -> Node c a
  NToken  :: forall {crep} (c :: TYPE crep) a a1.
             !(c -> Maybe a1) -> !(Node c a) -> Node c a
  NEmpty  :: forall {crep} (c :: TYPE crep) a.
             Node c a
  NAlt    :: forall {crep} (c :: TYPE crep) a.
             !(Node c a) -> !(Node c a) -> Node c a
-- Note that NGuard is lazy in the node. We have to introduce laziness in
-- at least one place, to make a graph with loops possible.

------------
-- Compile
------------

-- | \(O(m)\). Compile a @RE c a@ to a @Parser c a@.
--
-- Note: @compile@ does not limit the size of the @RE@. See 'compileBounded'
-- if you would like to limit the size.
-- @RE@s with size greater than @(maxBound::Int) \`div\` 2@ are not supported
-- and the behavior of such a @RE@ is undefined.
compile :: forall {crep} (c :: TYPE crep) a. RE c a -> Parser c a
compile re = evalState (compileToParser re) (Unique 0)

nxtU :: State Unique Unique
nxtU = state $ \u -> let !u' = Unique (unUnique u + 1) in (u, u')

compileToParser ::
  forall {crep} (c :: TYPE crep) a. RE c a -> State Unique (Parser c a)
compileToParser re = case re of
  RToken t -> pure $ PToken t
  RFmap st f re1 -> PFmap st f <$> compileToParser re1
  RFmap_ a re1 -> PFmap_ <$> compileToNode a re1
  RPure a -> pure $ PPure a
  RLiftA2 st f re1 re2 ->
    liftA2 (PLiftA2 st f) (compileToParser re1) (compileToParser re2)
  REmpty -> pure PEmpty
  RAlt re1 re2 -> do
    u <- nxtU
    liftA2 (PAlt u) (compileToParser re1) (compileToParser re2)
  RFold st gr f z re1 -> do
    u <- nxtU
    _localU <- nxtU
    case gr of
      Greedy -> PFoldGr u st f z <$> compileToParser re1
      Minimal -> PFoldMn u st f z <$> compileToParser re1
  RMany f1 f2 f z re1 -> do
    u <- nxtU
    _localU <- nxtU
    PMany u f1 f2 f z <$> compileToParser re1

compileToNode ::
  forall {crep} (c :: TYPE crep) a a1. a -> RE c a1 -> State Unique (Node c a)
compileToNode a re0 = go re0 (NAccept a)
  where
    go :: forall a2. RE c a2 -> Node c a -> State Unique (Node c a)
    go re nxt = case re of
      RToken t -> pure $ NToken t nxt
      RFmap _ _ re1 -> go re1 nxt
      RFmap_ _ re1 -> go re1 nxt
      RPure _ -> pure nxt
      RLiftA2 _ _ re1 re2 -> go re2 nxt >>= go re1
      REmpty -> pure NEmpty
      RAlt re1 re2 -> do
        u <- nxtU
        let nxt1 = NGuard u nxt
        liftA2 NAlt (go re1 nxt1) (go re2 nxt1)
      RFold _ gr _ _ re1 -> goMany gr re1 nxt
      RMany _ _ _ _ re1 -> goMany Greedy re1 nxt
    goMany :: forall a2.
              Greediness -> RE c a2 -> Node c a -> State Unique (Node c a)
    goMany gr re1 nxt = do
      u <- nxtU
      mfix $ \n -> do
        ndown <- go re1 n
        case gr of
           Greedy -> pure $ NGuard u (NAlt ndown nxt)
           Minimal -> pure $ NGuard u (NAlt nxt ndown)

--------------------
-- Compile bounded
--------------------

-- | \(O(\min(l,m))\). Compile a @RE c a@ to a @Parser c a@.
--
-- Returns @Nothing@ if the size of the @RE@ is greater than the provided limit
-- \(l\). You may want to use this if you suspect that the @RE@ may be too
-- large, for instance if the regex is constructed from an untrusted source.
--
-- While the exact size of a @RE@ depends on an internal representation, it can
-- be assumed to be in the same order as the length of a
-- [regex pattern](https://en.wikipedia.org/wiki/Regular_expression#Syntax)
-- corresponding to the @RE@.
compileBounded ::
  forall {crep} (c :: TYPE crep) a. Int -> RE c a -> Maybe (Parser c a)
compileBounded lim re =
  if checkSize lim re
  then Just $! compile re
  else Nothing

checkSize :: forall {crep} (c :: TYPE crep) a. Int -> RE c a -> Bool
checkSize lim re0 = isJust (evalStateT (go re0) 0)
  where
    go :: RE c a1 -> StateT Int Maybe ()
    go re = case re of
        RToken _ -> inc
        RFmap _ _ re1 -> inc *> go re1
        RFmap_ _ re1 -> inc *> go re1
        RPure _ -> inc
        RLiftA2 _ _ re1 re2 -> inc *> go re1 *> go re2
        REmpty -> inc
        RAlt re1 re2 -> inc *> go re1 *> go re2
        RMany _ _ _ _ re1 -> inc *> go re1
        RFold _ _ _ _ re1 -> inc *> go re1
    inc = do
      n <- get
      if n == lim
      then empty
      else put $! n+1

----------
-- Parse
----------

data Cont (c :: TYPE crep) b a where
  CTop     :: forall {crep} (c :: TYPE crep) a.
              Cont c a a
  CFmap    :: forall {crep} (c :: TYPE crep) b a a1.
              !Strictness -> !(b -> a1) -> !(Cont c a1 a) -> Cont c b a
  CFmap_   :: forall {crep} (c :: TYPE crep) b a a1.
              !(Node c a1) -> !(Cont c a1 a) -> Cont c b a
  CLiftA2A :: forall {crep} (c :: TYPE crep) b a a2 a3.
              !Strictness -> !(b -> a2 -> a3) -> !(Parser c a2) -> !(Cont c a3 a) -> Cont c b a
  CLiftA2B :: forall {crep} (c :: TYPE crep) b a a1 a3.
              !Strictness -> !(a1 -> b -> a3) -> a1 -> !(Cont c a3 a) -> Cont c b a
  CAlt     :: forall {crep} (c :: TYPE crep) b a.
              !Unique -> !(Cont c b a) -> Cont c b a
  CFoldGr  :: forall {crep} (c :: TYPE crep) b a a1.
              !Unique -> !Strictness -> !(Parser c b) -> !(a1 -> b -> a1) -> a1 -> !(Cont c a1 a) -> Cont c b a
  CFoldMn  :: forall {crep} (c :: TYPE crep) b a a1.
              !Unique -> !Strictness -> !(Parser c b) -> !(a1 -> b -> a1) -> a1 -> !(Cont c a1 a) -> Cont c b a
  CMany    :: forall {crep} (c :: TYPE crep) b a a1 a2.
              !Unique -> !(Parser c b) -> !(b -> a2) -> !(a1 -> a2) -> !(a1 -> b -> a1) -> !a1 -> !(Cont c a2 a) -> Cont c b a

data NeedC (c :: TYPE crep) a where
  NeedC :: forall {crep} (c :: TYPE crep) a b.
           (c -> Maybe b) -> !(Cont c b a) -> NeedC c a

data StepState (c :: TYPE crep) a where
  StepState :: forall {crep} (c :: TYPE crep) a.
    { sSet :: {-# UNPACK #-} !UniqueSet
    , sNeed :: ![NeedC c a]
    , sResult :: !(Maybe a)
    } -> StepState c a

stepStateZero :: forall {crep} (c :: TYPE crep) a. StepState c a
stepStateZero = StepState U.empty [] Nothing

-- Note: Ideally we would have
-- down :: Parser c b -> Cont c b a -> State (StepState c a) ()
-- and similar downNode and up, but GHC is unable to optimize it to be
-- equivalent to the current code.
--
-- Using State is pretty convenient though, so it is used in branches. This
-- seems to get optimized well enough.

sMember :: forall {crep} (c :: TYPE crep) a. Unique -> State (StepState c a) Bool
sMember u = gets $ \pt -> U.member u (sSet pt)

sInsert :: forall {crep} (c :: TYPE crep) a. Unique -> State (StepState c a) ()
sInsert u = modify' $ \pt -> pt { sSet = U.insert u (sSet pt) }

down ::
  forall {crep} (c :: TYPE crep) a b.
  Parser c b -> Cont c b a -> StepState c a -> StepState c a
down p !ct !pt = case p of
  PToken t -> pt { sNeed = NeedC t ct : sNeed pt }
  PFmap st f p1 -> down p1 (CFmap st f ct) pt
  PFmap_ n -> downNode n ct pt
  PPure b -> up b ct pt
  PLiftA2 st f p1 p2 -> down p1 (CLiftA2A st f p2 ct) pt
  PEmpty -> pt
  PAlt u p1 p2 ->
    let ct1 = CAlt u ct
    in down p2 ct1 $! down p1 ct1 pt
  PFoldGr u st f z p1 -> flip execState pt $
    unlessM (sMember u) $ do
      sInsert (localU u)
      modify' $ down p1 (CFoldGr u st p1 f z ct)
      unlessM (sMember u) $ do
        sInsert u
        modify' $ up z ct
  PFoldMn u st f z p1 -> flip execState pt $
    unlessM (sMember u) $ do
      unlessM (sMember (localU u)) $ do
        modify' $ up z ct
      sInsert u
      modify' $ down p1 (CFoldMn u st p1 f z ct)
  PMany u f1 f2 f z p1 -> flip execState pt $
    unlessM (sMember u) $ do
      sInsert (localU u)
      modify' $ down p1 (CMany u p1 f1 f2 f z ct)
      unlessM (sMember u) $ do
        sInsert u
        let !x = f2 z
        modify' $ up x ct

downNode ::
  forall {crep} (c :: TYPE crep) a b.
  Node c b -> Cont c b a -> StepState c a -> StepState c a
downNode n0 !ct = go n0
  where
    go n !pt = case n of
      NAccept b -> up b ct pt
      NGuard u n1
        | U.member u (sSet pt) -> pt
        | otherwise -> go n1 (pt { sSet = U.insert u (sSet pt) })
      NToken t nxt ->
        pt { sNeed = NeedC t (CFmap_ nxt ct) : sNeed pt }
      NEmpty -> pt
      NAlt n1 n2 -> go n2 $! go n1 pt

up ::
  forall {crep} (c :: TYPE crep) a b.
  b -> Cont c b a -> StepState c a -> StepState c a
up b ct !pt = case ct of
  CTop -> pt { sResult = sResult pt <|> Just b }
  CFmap st f ct1 -> case st of
    Strict -> let !x = f b in up x ct1 pt
    NonStrict -> up (f b) ct1 pt
  CFmap_ n ct1 -> downNode n ct1 pt
  CLiftA2A st f p1 ct1 -> down p1 (CLiftA2B st f b ct1) pt
  CLiftA2B st f a ct1 -> case st of
    Strict -> let !x = f a b in up x ct1 pt
    NonStrict -> up (f a b) ct1 pt
  CAlt u ct1 -> flip execState pt $
    unlessM (sMember u) $ do
      sInsert u
      modify' $ up b ct1
  CFoldGr u st p1 f z ct1 -> flip execState pt $
    unlessM (sMember u) $ do
      lc <- sMember (localU u)
      if lc then do
        sInsert u
        modify' $ up z ct1
      else do
        let go z1 = do
              modify' $ down p1 (CFoldGr u st p1 f z1 ct1)
              sInsert u
              modify' $ up z1 ct1
            {-# INLINE go #-}
        case st of
          Strict -> let !z1 = f z b in go z1
          NonStrict -> go (f z b)
  CFoldMn u st p1 f z ct1 -> flip execState pt $
    unlessM (sMember u) $ do
      let go z1 = do
            sInsert (localU u)
            modify' $ up z1 ct1
            unlessM (sMember u) $ do
              sInsert u
              modify' $ down p1 (CFoldMn u st p1 f z1 ct1)
          {-# INLINE go #-}
      case st of
        Strict -> let !z1 = f z b in go z1
        NonStrict -> go (f z b)
  CMany u p1 f1 f2 f z ct1 -> flip execState pt $
    unlessM (sMember u) $ do
      lc <- sMember (localU u)
      if lc then do
        sInsert u
        let !x = f1 b
        modify' $ up x ct1
      else do
        let !z1 = f z b
        modify' $ down p1 (CMany u p1 f1 f2 f z1 ct1)
        sInsert u
        let !x = f2 z1
        modify' $ up x ct1

localU :: Unique -> Unique
localU = Unique . (+1) . unUnique

--------------------
-- Running a Parser
--------------------

-- | The state maintained by a parser.
data ParserState (c :: TYPE crep) a where
  ParserState :: forall {crep} (c :: TYPE crep) a.
    { psNeed :: ![NeedC c a]
    , psResult :: !(Maybe a)
    } -> ParserState c a

-- | \(O(m \log m)\). Prepare a parser for input.
prepareParser :: forall {crep} (c :: TYPE crep) a. Parser c a -> ParserState c a
prepareParser p = toParserState (down p CTop stepStateZero)

-- | \(O(m \log m)\). Step a parser by feeding a single element @c@. Returns
-- @Nothing@ if the parse has failed regardless of further input. Otherwise,
-- returns an updated @ParserState@.
stepParser :: ParserState c a -> c -> Maybe (ParserState c a)
stepParser ps c =
  if null (psNeed ps)
  then Nothing
  else
    let f pt (NeedC t ct) = maybe pt (\b -> up b ct pt) (t c)
    in Just $ toParserState $ F.foldl' f stepStateZero (psNeed ps)
{-# INLINE stepParser #-}

-- | \(O(1)\). Get the parse result for the input fed into the parser so far.
finishParser :: forall {crep} (c :: TYPE crep) a. ParserState c a -> Maybe a
finishParser = psResult

toParserState :: forall {crep} (c :: TYPE crep) a. StepState c a -> ParserState c a
toParserState pt = ParserState
  { psNeed = reverse (sNeed pt)
  , psResult = sResult pt
  }

-- | A fold function.
type Foldr f a = forall b. (a -> b -> b) -> b -> f -> b

-- | \(O(mn \log m)\). Run a parser given a sequence @f@ and a fold of @f@.
parseFoldr :: Foldr f c -> Parser c a -> f -> Maybe a
parseFoldr fr = \p xs -> fr f finishParser xs (prepareParser p)
  where
    f c k = \ps -> stepParser ps c >>= k
{-# INLINE parseFoldr #-}

-- | \(O(m \log m)\). Step a parser by feeding a single element @c@. Returns
-- @Nothing@ if the parse has failed regardless of further input. Otherwise,
-- returns an updated @ParserState@.
--
-- Like 'stepParser' but allows for levity-polymorphic @c@.
stepParserK ::
  forall {crep} (c :: TYPE crep) a.
  ParserState c a -> (forall r. (c -> r) -> r) -> Maybe (ParserState c a)
stepParserK ps c =
  if null (psNeed ps)
  then Nothing
  else
    let f pt (NeedC t ct) = maybe pt (\b -> up b ct pt) (c t)
    in Just $ toParserState $ F.foldl' f stepStateZero (psNeed ps)
{-# INLINE stepParserK #-}

-- | A fold function.
type FoldrK f (a :: TYPE arep) =
  forall b. ((forall r. (a -> r) -> r) -> b -> b) -> b -> f -> b

-- | \(O(mn \log m)\). Run a parser given a sequence @f@ and a fold of @f@.
--
-- Like 'parseFoldr' but allows for levity-polymorphic @c@.
parseFoldrK ::
  forall {crep} (c :: TYPE crep) f a. FoldrK f c -> Parser c a -> f -> Maybe a
parseFoldrK fr = \p xs -> fr f finishParser xs (prepareParser p)
  where
    f (c :: forall r. (c -> r) -> r) k = \ps -> stepParserK ps c >>= k
{-# INLINE parseFoldrK #-}

---------
-- Util
---------

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb mx = do
  b <- mb
  if b then pure () else mx

----------
-- Notes
----------

-- Note [About the algorithm]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- To parse using a regex, we compile the regex into a non-deterministic finite
-- automata (NFA). Actually, we only only do this for recognition, i.e. checking
-- whether a sequence satisfies a regex. This is done if the regex is a RFmap_.
--
-- To parse into a value, we have to do more work. We keep the regex as a tree
-- (Parser), but we preserve the path taken down the tree, like a zipper.
-- This lets us go up the tree and continue parsing, once we parse a c.
-- If you squint your eyes, this is also an NFA, only each edge of the NFA is
-- broken into multiple steps up and down the tree.
--
-- Recognition using the NFA is faster than parsing, unsurprisingly.
-- A Parser tree can have NFAs as children. This means that if some subtree of
-- the regex only attempts to recognize some input, it doesn't pay the extra
-- cost of parsing.
--
-- Key objective: O(mn log m) time parsing. This means that for every c fed into
-- the parser, we are allowed to take no more than O(m log m) time.
--
-- How is this ensured?
-- 1. The compiled regex must have O(m) nodes and O(m) edges. The Parser tree
--    satisfies this, of course, since it reflects the regex itself. The NFA
--    also satisfies this, implemented as Thompson's construction:
--    https://en.wikipedia.org/wiki/Thompson%27s_construction
-- 2. For every c, no edge is traversed twice. Tree edges are bidirectional
--    unlike NFA edges, so an NFA edge may be traversed only once and a tree
--    edge may be traversed once in each direction.
--
-- NFA guards: To ensure each NFA edge can be traversed only once, guard nodes
-- (NGuard) carry a Unique which can be stored in a set (sSet). Guard nodes are
-- created during compilation whenever two nodes would lead into one node:
-- A->C, B->C. A guard node is added, such that it becomes A->G, B->G, G->C.
--
-- Parser guards: Parser guards are more tricky.
-- Alt: There are two ways into an Alt node when going up. So, an Alt node
--   carries a Unique is stored in sSet and guards upward travel through the
--   node.
-- FoldGr: There are two ways into a FoldGr node, one going down and one going
--   up. But, we can't just a use a Unique to guard entry into it because we
--   want to handle loopy cases correctly! A loopy case is where we reach the
--   same node in the tree by going up and down the edges without consuming
--   input. To detect this, we use a separate Unique (localU) when going down.
--   If we find it set when going up, we are looping. When we send up a value,
--   looping or not, we guard entry into the node using its (not localU) Unique.
-- Many: A Many node is just like FoldlGr, only the looping case is handled
--   specially.
-- FoldMn: Like FoldGr, there are two ways into a FoldlMn node, one going down
--   and one going up, and we must handle loopy cases correctly. A FoldMn sends
--   a value up before going down. So, the localU is set when going up and if
--   we find it when going down, we are looping. When we send down a value, we
--   guard entry into the node using its (not localU) Unique.
