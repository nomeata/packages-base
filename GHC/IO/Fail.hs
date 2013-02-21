{-# LANGUAGE NoImplicitPrelude #-}
module GHC.IO.Fail where

import GHC.Base
import GHC.Exception
import Data.Typeable
import GHC.Show


-- | This exception is thrown by the 'fail' method of the 'Monad' 'IO' instance.
--
--   The Exception instance of IOException will also catch this, converting the
--   IOFail to a UserError, for compatibility and consistency with the Haskell
--   report
data IOFail = IOFail String

instance Typeable IOFail
instance Show IOFail
instance Exception IOFail

