{-# LANGUAGE CPP #-}

module Regex.Internal.Array
  ( SmallArray
  , emptySmallArray
  , smallArrayFromList
  , foldlSmallArray'
  ) where

import qualified Data.Foldable as F

#ifdef __GLASGOW_HASKELL__
import Data.Primitive.SmallArray
  (SmallArray, emptySmallArray, smallArrayFromList)

#else
type SmallArray = []

emptySmallArray :: SmallArray a
emptySmallArray = []

smallArrayFromList :: [a] -> SmallArray a
smallArrayFromList = id
#endif

foldlSmallArray' :: (b -> a -> b) -> b -> SmallArray a -> b
foldlSmallArray' = F.foldl'
