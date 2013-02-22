{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, ForeignFunctionInterface #-}

module GHC.Event.Clock (getMonotonicTime) where

import GHC.Base
import GHC.Err
import GHC.IO
import GHC.Real
import GHC.Float
import GHC.Num
import Data.Word

-- | Return monotonic time in seconds, since some unspecified starting point
getMonotonicTime :: IO Double
getMonotonicTime = do w <- getMonotonicNSec
                      return (fromIntegral w / fromInteger 1000000000)

--foreign import ccall unsafe "getMonotonicNSec"
--    getMonotonicNSec :: IO Word64
getMonotonicNSec :: IO Word64
getMonotonicNSec = undefined

