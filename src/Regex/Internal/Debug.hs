{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Semigroup
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
-- tested against the 'token' functions in the 'RE' and accepted characters
-- displayed.
reToDot :: forall c a. Maybe ([c], [c] -> String) -> RE c a -> String
reToDot ma re0 = execM $ do
  writeLn $ showString "digraph RE {"
  _ <- go re0
  writeLn $ showString "}"
  where
    go :: forall b. RE c b -> M Id
    go re = case re of
      RToken t -> new $ labelToken "RToken" t ma
      RFmap st _ re1 ->
        withNew (showString "RFmap" ... dispsSt st) $ \i ->
          go re1 >>= writeEdge i
      RFmap_ _ re1 ->
        withNew (showString "RFmap_") $ \i ->
          go re1 >>= writeEdge i
      RPure _ -> new $ showString "RPure"
      RLiftA2 st _ re1 re2 ->
        withNew (showString "RLiftA2" ... dispsSt st) $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      REmpty -> new $ showString "REmpty"
      RAlt re1 re2 ->
        withNew (showString "RAlt") $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      RFold st gr _ _ re1 ->
        withNew (showString "RFold" ... dispsSt st ... dispsGr gr) $ \i ->
          go re1 >>= writeEdge i
      RMany _ _ _ _ re1 ->
        withNew (showString "RMany") $ \i ->
          go re1 >>= writeEdge i

-----------
-- Parser
-----------

-- | Generate a [Graphviz DOT](https://graphviz.org/doc/info/lang.html)
-- visualization of a 'Parser'. Optionally takes an alphabet @[c]@, which will
-- be tested against the 'token' functions in the 'Parser' and the accepted
-- characters displayed.
parserToDot :: forall c a. Maybe ([c], [c] -> String) -> Parser c a -> String
parserToDot ma p0 = execM $ do
  writeLn $ showString "digraph Parser {"
  _ <- go p0
  writeLn $ showString "}"
  where
    go :: forall b. Parser c b -> M Id
    go p = case p of
      PToken t -> new $ labelToken "PToken" t ma
      PFmap st _ re1 ->
        withNew (showString "PFmap" ... dispsSt st) $ \i ->
          go re1 >>= writeEdge i
      PFmap_ node ->
        withNew (showString "PFmap_") $ \i -> do
          writeLn $ showString ("subgraph cluster" ++ unId i ++ " {")
          j <- evalStateT (goNode node) IM.empty
          writeLn $ showString "}"
          writeEdge i j
      PPure _ -> new $ showString "PPure"
      PLiftA2 st _ re1 re2 ->
        withNew (showString "PLiftA2" ... dispsSt st) $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      PEmpty -> new $ showString "PEmpty"
      PAlt _ re1 re2 res ->
        withNew (showString "PAlt") $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
          F.traverse_ (go >=> writeEdge i) res
      PMany _ _ _ _ _ re1 ->
        withNew (showString "PMany") $ \i ->
          go re1 >>= writeEdge i
      PFoldGr _ st _ _ re1 ->
        withNew (showString "PFoldGr" ... dispsSt st) $ \i ->
          go re1 >>= writeEdge i
      PFoldMn _ st _ _ re1 ->
        withNew (showString "PFoldMn" ... dispsSt st) $ \i ->
          go re1 >>= writeEdge i

    goNode :: forall b. Node c b -> StateT (IntMap Id) M Id
    goNode n = case n of
      NAccept _ -> lift $ new $ showString "NAccept"
      NGuard u n1 -> do
        v <- gets $ IM.lookup (unUnique u)
        case v of
          Just i -> pure i
          Nothing -> withNewT (showString "NGuard") $ \i -> do
            modify' $ IM.insert (unUnique u) i
            goNode n1 >>= lift . writeEdge i
      NToken t n1 ->
        withNewT (labelToken "NToken" t ma) $ \i ->
          goNode n1 >>= lift . writeEdge i
      NEmpty -> lift $ new $ showString "NEmpty"
      NAlt n1 n2 ns -> withNewT (showString "NAlt") $ \i -> do
        goNode n1 >>= lift . writeEdge i
        goNode n2 >>= lift . writeEdge i
        F.traverse_ (goNode >=> lift . writeEdge i) ns

------------------
-- Display Chars
------------------

dispCharRanges :: [Char] -> String
dispCharRanges = show . CS.ranges . CS.fromList

-----------------
-- Common stuff
-----------------

dispsSt :: Strictness -> ShowS
dispsSt st = showString $ case st of
  Strict -> "S"
  NonStrict -> "NS"

dispsGr :: Greediness -> ShowS
dispsGr gr = showString $ case gr of
  Greedy -> "G"
  Minimal -> "M"

labelToken :: String -> (c -> Maybe a) -> Maybe ([c], [c] -> String) -> ShowS
labelToken node t = maybe
  (showString node)
  (\(cs, disp) -> showString node ...
                  (showString . escape . disp) (filter (isJust . t) cs))

escape :: String -> String
escape = init . tail . show

(...) :: ShowS -> ShowS -> ShowS
s1 ... s2 = s1 . showChar ' ' . s2
infixr 9 ...

declNode :: Id -> ShowS -> ShowS
declNode i label =
  showString (unId i) ...
  showString "[label=\"" .
  label .
  showString "\", ordering=\"out\"]"

type M = StateT Int (Writer (Endo String))

execM :: M a -> String
execM = ($ "") . appEndo . execWriter . flip runStateT 1

newtype Id = Id { unId :: String }

nxt :: M Id
nxt = state $ \i -> let !i' = i+1 in (Id (show i), i')

writeLn :: ShowS -> M ()
writeLn = lift . tell . Endo . (. showChar '\n')

writeEdge :: Id -> Id -> M ()
writeEdge fr to = writeLn $ showString (unId fr ++ " -> " ++ unId to)

new :: ShowS -> M Id
new node = do
  i <- nxt
  writeLn $ declNode i node
  pure i

withNew :: ShowS -> (Id -> M a) -> M Id
withNew node f = runIdentityT $ withNewT node $ lift . f

withNewT :: (MonadTrans t, Monad (t M)) => ShowS -> (Id -> t M a) -> t M Id
withNewT node f = do
  i <- lift $ new node
  _ <- f i
  pure i
