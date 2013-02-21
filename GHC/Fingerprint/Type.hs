{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- ----------------------------------------------------------------------------
-- 
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- ----------------------------------------------------------------------------

module GHC.Fingerprint.Type (Fingerprint(..)) where

import GHC.Base
import GHC.Word

-- Using 32-bit FNV hashes

data Fingerprint = Fingerprint {-# UNPACK #-} !Word32
  deriving (Eq, Ord)

