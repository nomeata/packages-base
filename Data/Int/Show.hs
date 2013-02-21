{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Odds and ends, mostly functions for reading and showing
-- 'RealFloat'-like kind of values.
--
-----------------------------------------------------------------------------

module Data.Int.Show (

        -- * Showing

        showSigned,       -- :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS

        showIntAtBase,    -- :: Integral a => a -> (a -> Char) -> a -> ShowS
        showInt,          -- :: Integral a => a -> ShowS
        showHex,          -- :: Integral a => a -> ShowS
        showOct,          -- :: Integral a => a -> ShowS
        ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Real
import GHC.Num
import GHC.Show
import Data.Maybe
#else
import Data.Char
#endif

#ifdef __HUGS__
import Hugs.Prelude
import Hugs.Numeric
#endif

#ifdef __GLASGOW_HASKELL__
-- -----------------------------------------------------------------------------
-- Showing

-- | Show /non-negative/ 'Integral' numbers in base 10.
showInt :: Integral a => a -> ShowS
showInt n0 cs0
    | n0 < fromInteger 0    = error "Numeric.showInt: can't show negative numbers"
    | otherwise = go n0 cs0
    where
    go n cs
        | n < fromInteger 10    = case unsafeChr (ord '0' + fromIntegral n) of
            c@(C# _) -> c:cs
        | otherwise = case unsafeChr (ord '0' + fromIntegral r) of
            c@(C# _) -> go q (c:cs)
        where
        (q,r) = n `quotRem` fromInteger 10

#endif  /* __GLASGOW_HASKELL__ */

-- ---------------------------------------------------------------------------
-- Integer printing functions

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
showIntAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n0 r0
  | base <= fromInteger 1 = error ("Numeric.showIntAtBase: applied to unsupported base " ++ show base)
  | n0 <  fromInteger 0   = error ("Numeric.showIntAtBase: applied to negative number " ++ show n0)
  | otherwise = showIt (quotRem n0 base) r0
   where
    showIt (n,d) r = seq c $ -- stricter than necessary
      if n == fromInteger 0 then r' else showIt (quotRem n base) r'
     where
      c  = toChr (fromIntegral d)
      r' = c : r

-- | Show /non-negative/ 'Integral' numbers in base 16.
showHex :: (Integral a,Show a) => a -> ShowS
showHex = showIntAtBase (fromInteger 16) intToDigit

-- | Show /non-negative/ 'Integral' numbers in base 8.
showOct :: (Integral a, Show a) => a -> ShowS
showOct = showIntAtBase (fromInteger 8)  intToDigit
