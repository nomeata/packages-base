{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.C.Puts (puts) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import GHC.Base
import GHC.IO
import GHC.Real
import GHC.Err
import GHC.Word
import GHC.Num

-- from  System.Posix.Internals 
puts :: String -> IO ()
puts s = withCAStringLen (s ++ "\n") $ \(p, len) -> do
            -- In reality should be withCString, but assume ASCII to avoid loop
            -- if this is called by GHC.Foreign
           _ <- c_write (fromInteger 1) (castPtr p) (fromIntegral len)
           return ()

-- See Note: CSsize
--foreign import capi unsafe "HsBase.h write"
--   c_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_write :: CInt -> Ptr Word8 -> CSize -> IO ()
c_write = undefined

