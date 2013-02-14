{-# LANGUAGE BangPatterns, CPP #-}

module Prelude where

import GHC.Base

($!)    :: (a -> b) -> a -> b
#ifdef __GLASGOW_HASKELL__
f $! x  = let !vx = x in f vx  -- see #2273
#elif !defined(__HUGS__)
f $! x  = x `seq` f x
#endif



