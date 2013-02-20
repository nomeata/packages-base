{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.C.Puts where

import GHC.Base
import GHC.IO

-- from  System.Posix.Internals 
puts :: String -> IO ()
