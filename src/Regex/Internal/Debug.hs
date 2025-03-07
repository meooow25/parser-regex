{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions for visualizing @RE@s and @Parser@s.
-- [See here](https://github.com/meooow25/parser-regex/wiki/Visualizations)
-- for some examples.
--
module Regex.Internal.Debug
  ( reToDot
  , parserToDot
  , dispCharRanges
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import qualified Data.Foldable as F
import Data.Maybe (isJust)
import Data.String
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Regex.Internal.Regex (RE(..), Strictness(..), Greediness(..))
import Regex.Internal.Parser (Node(..), Parser(..))
import Regex.Internal.Unique (Unique(..))
import qualified Regex.Internal.CharSet as CS

-------
-- RE
-------

-- | Generate a [Graphviz DOT](https://graphviz.org/doc/info/lang.html)
-- visualization of a 'RE'. Optionally takes an alphabet @[c]@, which will be
-- tested against the @token@ functions in the 'RE' and accepted characters
-- displayed.
reToDot :: forall c a. Maybe ([c], [c] -> String) -> RE c a -> String
reToDot ma re0 = execM $ do
  writeLn "digraph RE {"
  _ <- go re0
  writeLn "}"
  where
    go :: forall b. RE c b -> M Id
    go re = case re of
      RToken t -> new $ labelToken "RToken" t ma
      RFmap st _ re1 ->
        withNew ("RFmap" <+> dispsSt st) $ \i ->
          go re1 >>= writeEdge i
      RFmap_ _ re1 ->
        withNew "RFmap_" $ \i ->
          go re1 >>= writeEdge i
      RPure _ -> new "RPure"
      RLiftA2 st _ re1 re2 ->
        withNew ("RLiftA2" <+> dispsSt st) $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      REmpty -> new "REmpty"
      RAlt re1 re2 ->
        withNew "RAlt" $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      RFold st gr _ _ re1 ->
        withNew ("RFold" <+> dispsSt st <+> dispsGr gr) $ \i ->
          go re1 >>= writeEdge i
      RMany _ _ _ _ re1 ->
        withNew "RMany" $ \i ->
          go re1 >>= writeEdge i

-----------
-- Parser
-----------

-- | Generate a [Graphviz DOT](https://graphviz.org/doc/info/lang.html)
-- visualization of a 'Parser'. Optionally takes an alphabet @[c]@, which will
-- be tested against the @token@ functions in the 'Parser' and the accepted
-- characters displayed.
parserToDot :: forall c a. Maybe ([c], [c] -> String) -> Parser c a -> String
parserToDot ma p0 = execM $ do
  writeLn "digraph Parser {"
  _ <- go p0
  writeLn "}"
  where
    go :: forall b. Parser c b -> M Id
    go p = case p of
      PToken t -> new $ labelToken "PToken" t ma
      PFmap st _ re1 ->
        withNew ("PFmap" <+> dispsSt st) $ \i ->
          go re1 >>= writeEdge i
      PFmap_ node ->
        withNew "PFmap_" $ \i -> do
          writeLn $ "subgraph cluster" <> idStr i <> " {"
          j <- evalStateT (goNode node) IM.empty
          writeLn "}"
          writeEdge i j
      PPure _ -> new "PPure"
      PLiftA2 st _ re1 re2 ->
        withNew ("PLiftA2" <+> dispsSt st) $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      PEmpty -> new "PEmpty"
      PAlt _ re1 re2 res ->
        withNew "PAlt" $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
          F.traverse_ (go >=> writeEdge i) res
      PMany _ _ _ _ _ re1 ->
        withNew "PMany" $ \i ->
          go re1 >>= writeEdge i
      PFoldGr _ st _ _ re1 ->
        withNew ("PFoldGr" <+> dispsSt st) $ \i ->
          go re1 >>= writeEdge i
      PFoldMn _ st _ _ re1 ->
        withNew ("PFoldMn" <+> dispsSt st) $ \i ->
          go re1 >>= writeEdge i

    goNode :: forall b. Node c b -> StateT (IntMap Id) M Id
    goNode n = case n of
      NAccept _ -> lift $ new "NAccept"
      NGuard u n1 -> do
        v <- gets $ IM.lookup (unUnique u)
        case v of
          Just i -> pure i
          Nothing -> withNewT "NGuard" $ \i -> do
            modify' $ IM.insert (unUnique u) i
            goNode n1 >>= lift . writeEdge i
      NToken t n1 ->
        withNewT (labelToken "NToken" t ma) $ \i ->
          goNode n1 >>= lift . writeEdge i
      NEmpty -> lift $ new "NEmpty"
      NAlt n1 n2 ns -> withNewT "NAlt" $ \i -> do
        goNode n1 >>= lift . writeEdge i
        goNode n2 >>= lift . writeEdge i
        F.traverse_ (goNode >=> lift . writeEdge i) ns

------------------
-- Display Chars
------------------

-- |
-- >>> dispCharRanges "abc012def"
-- "[('0','2'),('a','f')]"
dispCharRanges :: [Char] -> String
dispCharRanges = show . CS.ranges . CS.fromList

-----------------
-- Common stuff
-----------------

newtype Str = Str { runStr :: String -> String }

instance IsString Str where
  fromString = Str . (++)

instance Semigroup Str where
  s1 <> s2 = Str (runStr s1 . runStr s2)

instance Monoid Str where
  mempty = Str id

dispsSt :: Strictness -> Str
dispsSt st = case st of
  Strict -> "S"
  NonStrict -> "NS"

dispsGr :: Greediness -> Str
dispsGr gr = case gr of
  Greedy -> "G"
  Minimal -> "M"

labelToken :: String -> (c -> Maybe a) -> Maybe ([c], [c] -> String) -> Str
labelToken node t = maybe
  (fromString node)
  (\(cs, disp) ->
    fromString node <+>
    (fromString . escape . disp) (filter (isJust . t) cs))

escape :: String -> String
escape = init . tail' . show
  where
    tail' (_:xs) = xs
    tail' [] = error "tail'"

(<+>) :: Str -> Str -> Str
s1 <+> s2 = s1 <> " " <> s2
infixr 6 <+>

declNode :: Id -> Str -> Str
declNode i label =
  idStr i <+>
  "[label=\"" <>
  label <>
  "\", ordering=\"out\"]"

type M = StateT Int (Writer Str)

execM :: M a -> String
execM = ($ "") . runStr . execWriter . flip runStateT 1

newtype Id = Id { unId :: String }

idStr :: Id -> Str
idStr = fromString . unId

nxt :: M Id
nxt = state $ \i -> let !i' = i+1 in (Id (show i), i')

writeLn :: Str -> M ()
writeLn = lift . tell . (<> "\n")

writeEdge :: Id -> Id -> M ()
writeEdge fr to = writeLn $ idStr fr <> " -> " <> idStr to

new :: Str -> M Id
new node = do
  i <- nxt
  writeLn $ declNode i node
  pure i

withNew :: Str -> (Id -> M a) -> M Id
withNew node f = runIdentityT $ withNewT node $ lift . f

withNewT :: (MonadTrans t, Monad (t M)) => Str -> (Id -> t M a) -> t M Id
withNewT node f = do
  i <- lift $ new node
  _ <- f i
  pure i
