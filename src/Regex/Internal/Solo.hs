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
  , solo
  , flipSolo
  ) where

mkSolo :: a -> Solo a
mkSolo' :: a -> Solo a
solo :: (a -> b) -> Solo a -> b
flipSolo :: Solo a -> (a -> b) -> b

#ifdef __GLASGOW_HASKELL__
newtype Solo a = Solo (# a #)
mkSolo x = Solo (# x #)
mkSolo' !x = Solo (# x #)
solo f (Solo (# x #)) = f x
flipSolo (Solo (# x #)) f = f x
#else
data Solo a = Solo a
mkSolo = Solo
mkSolo' !x = Solo x
solo f (Solo x) = f x
flipSolo (Solo x) f = f x
#endif
