{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
#endif

module Regex.Internal.Solo
  ( Solo
  , mkSolo
  , mkSolo'
  , matchSolo
  ) where

mkSolo :: a -> Solo a
mkSolo' :: a -> Solo a
matchSolo :: Solo a -> (a -> b) -> b

#ifdef __GLASGOW_HASKELL__
newtype Solo a = Solo (# a #)
mkSolo x = Solo (# x #)
mkSolo' !x = Solo (# x #)
matchSolo (Solo (# x #)) f = f x
#else
data Solo a = Solo a
mkSolo = Solo
mkSolo' !x = Solo x
matchSolo (Solo x) f = f x
#endif
