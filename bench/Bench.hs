{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Random.Strict
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import Numeric

import Test.Tasty.Bench

import qualified Regex.Text as RT
import qualified Regex.Base as RB

main :: IO ()
main = defaultMain
  [ textBenches
  ]

---------
-- Text
---------

textBenches :: Benchmark
textBenches = bgroup "Text"
  [ bgroup "char"
    [ env (pure aText) $ \data_ ->
      bench "a" $ whnf (RT.parse aP) data_
    ]
  , bgroup "charIgnoreCase"
    [ env (pure aAText) $ \data_ ->
      bench "aA" $ whnf (RT.parse aAP) data_
    ]
  , bgroup "text"
    [ env (pure aText) $ \data_ ->
      bench "a" $ whnf (RT.parse aP2) data_
    ]
  , bgroup "textIgnoreCase"
    [ env (pure aAText) $ \data_ ->
      bench "aA" $ whnf (RT.parse aAP2) data_
    ]
  , bgroup "naturalDec"
    [ env (pure bigNumberDec) $ \data_ ->
      bench "bigNumberDec" $ whnf (RT.parse natP) data_
    ]
  , bgroup "wordDecN"
    [ env (pure manyNumbers) $ \data_ ->
      bench "manyNumbers" $ whnf (RT.parseSure allWord19P) data_
    ]
  , bgroup "wordRangeDec"
    [ bgroup "parse"
      [ env (pure manyNumbers) $ \data_ ->
        bench "manyNumbers" $ whnf (RT.parseSure allWordP) data_
      ]
    , bgroup "compile"
      [ env (pure manyRanges) $ \data_ ->
        bench "manyRanges" $
        whnf (foldr (\rng -> (RT.wordRangeDec rng `seq`)) ()) data_
      ]
    ]
  , bgroup "naturalHex"
    [ env (pure bigNumberHex) $ \data_ ->
        bench "bigNumberHex" $ whnf (RT.parse natHexP) data_
    ]
  , bgroup "wordHexN"
    [ env (pure manyNumbersHex) $ \data_ ->
      bench "manyNumbersHex" $ whnf (RT.parseSure allWordHex16P) data_
    ]
  , bgroup "wordRangeHex"
    [ bgroup "parse"
      [ env (pure manyNumbersHex) $ \data_ ->
        bench "manyNumbersHex" $ whnf (RT.parseSure allWordHexP) data_
      ]
    , bgroup "compile"
      [ env (pure manyRanges) $ \data_ ->
        bench "manyRanges" $
        whnf (foldr (\rng -> (RT.wordRangeHex rng `seq`)) ()) data_
      ]
    ]
  , bgroup "chainl"
    [ env (pure bigExpr) $ \data_ ->
      bench "bigExpr" $ whnf (RT.parse exprlP) data_
    ]
  , bgroup "chainr"
    [ env (pure bigExpr) $ \data_ ->
      bench "bigExpr" $ whnf (RT.parse exprrP) data_
    ]
  , bgroup "bigRegex"
    [ env (pure aText1000) $ \t ->
      bench "bigRegex" $ whnf (RT.parse bigRegexP) t
    ]
  , env (pure aText) $ \t ->
    bgroup "many, linear time"
    [ bench "many" $ whnf (RT.parse manyAP) t
    , bench "manyMin" $ whnf (RT.parse manyMinAP) t
    , bench "manyr" $ whnf (RT.parse manyrAP) t
    , bench "toMatch many" $ whnf (RT.parse toMatchManyAP) t
    , bench "toMatch manyMin" $ whnf (RT.parse toMatchManyMinAP) t
    , bench "toMatch manyr" $ whnf (RT.parse toMatchManyrAP) t
    , bench "withMatch many" $ whnf (RT.parse withMatchManyAP) t
    , bench "withMatch manyMin" $ whnf (RT.parse withMatchManyMinAP) t
    , bench "withMatch manyr" $ whnf (RT.parse withMatchManyrAP) t
    ]
  ]
  where
    !aP = RT.compile $ RB.foldlMany' (\_ _ -> ()) () (RT.char 'a')
    !aAP = RT.compile $ RB.foldlMany' (\_ _ -> ()) () (RT.charIgnoreCase 'a')
    !aP2 = RT.compile $
      RB.foldlMany' (\_ _ -> ()) () (RT.text (T.replicate 10 "a"))
    !aAP2 = RT.compile $
      RB.foldlMany' (\_ _ -> ()) () (RT.textIgnoreCase (T.replicate 5 "Aa"))
    !natP = RT.compile $ RT.manyMin RT.anyChar *> RT.naturalDec
    !natHexP = RT.compile $ RT.manyMin RT.anyChar *> RT.naturalHex
    !allWordP = RT.compile $
      RB.toFindMany (RT.wordRangeDec (1, maxBound :: Word))
    !allWordHexP = RT.compile $
      RB.toFindMany (RT.wordRangeHex (1, 10 ^ (19 :: Int)))
    !allWord19P = RT.compile $ RB.toFindMany (RT.wordDecN 19)
    !allWordHex16P = RT.compile $ RB.toFindMany (RT.wordHexN 16)
    !exprlP = RT.compile $ RT.chainl1 RT.naturalDec ((+) <$ RT.char '+')
    !exprrP = RT.compile $ RT.chainl1 RT.naturalDec ((+) <$ RT.char '+')
    !bigRegexP = RT.compile $
      RB.foldlMany' (\_ _ -> ())
                    ()
                    (F.asum (map (<$ RT.char 'a') [1 :: Int .. 2000]))
    !manyAP = RT.compile $ many (RT.char 'a')
    !manyMinAP = RT.compile $ RT.manyMin (RT.char 'a')
    !manyrAP = RT.compile $ RT.manyr (RT.char 'a')
    !toMatchManyAP = RT.compile $ RT.toMatch $ many (RT.char 'a')
    !toMatchManyMinAP = RT.compile $ RT.toMatch $ RT.manyMin (RT.char 'a')
    !toMatchManyrAP = RT.compile $ RT.toMatch $ RT.manyr (RT.char 'a')
    !withMatchManyAP = RT.compile $ RT.withMatch $ many (RT.char 'a')
    !withMatchManyMinAP = RT.compile $ RT.withMatch $ RT.manyMin (RT.char 'a')
    !withMatchManyrAP = RT.compile $ RT.withMatch $ RT.manyr (RT.char 'a')

aText :: Text
aText = T.replicate 1000000 "a"

aAText :: Text
aAText = T.replicate 500000 "aA"

aText1000 :: Text
aText1000 = T.replicate 1000 "a"

bigNumberDec :: Text
bigNumberDec = T.pack $ evalR $
  liftA2 (:) (getRandomR ('1','9')) (replicateM 1000000 (getRandomR ('0','9')))

bigNumberHex :: Text
bigNumberHex = T.pack $ evalR $
  liftA2 (:) (d '1') (replicateM 1000000 (d '0'))
  where
    d l = oneof (map getRandomR [(l,'9'), ('a','f'), ('A','F')])

manyNumbers :: Text
manyNumbers =
  T.intercalate " " $
  map (T.pack . show) $
  evalR $
  replicateM 1000 (getRandom :: R Word)

manyNumbersHex :: Text
manyNumbersHex =
  T.intercalate " " $
  map (T.pack . ($ "") . showHex) $
  evalR $
  replicateM 1000 (getRandom :: R Word)

manyRanges :: [(Word, Word)]
manyRanges = evalR $ replicateM 100000 getRandom

bigExpr :: Text
bigExpr = T.intercalate "+" $ map (T.pack . show) . evalR $
  replicateM 100000 (getRandom :: R Word)

-----------
-- Random
-----------

type R = Rand StdGen

oneof :: [R a] -> R a
oneof [] = error "!"
oneof rs = do
  let n = length rs
  i <- getRandomR (0, n-1)
  rs !! i

evalR :: R a -> a
evalR = flip evalRand (mkStdGen 42)
