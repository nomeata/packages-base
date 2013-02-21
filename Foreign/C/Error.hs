{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -#include "HsBase.h" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- C-specific Marshalling support: Handling of C \"errno\" error codes.
--
-----------------------------------------------------------------------------

module Foreign.C.Error (

  -- * Haskell representations of @errno@ values

  Errno(..),            -- instance: Eq

  -- ** Common @errno@ symbols
  -- | Different operating systems and\/or C libraries often support
  -- different values of @errno@.  This module defines the common values,
  -- but due to the open definition of 'Errno' users may add definitions
  -- which are not predefined.
  eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,

  -- ** 'Errno' functions
                        -- :: Errno
  isValidErrno,         -- :: Errno -> Bool

  -- access to the current thread's "errno" value
  --
  getErrno,             -- :: IO Errno
  resetErrno,           -- :: IO ()

  ErrnoError(..),
  -- throw current "errno" value
  --
  throwErrno,           -- ::                String               -> IO a

  -- ** Guards for IO operations that may fail

  throwErrnoIf,         -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIf_,        -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfRetry,    -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIfRetry_,   -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfMinus1,   -- :: Num a 
                        -- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1_,  -- :: Num a 
                        -- =>                String -> IO a       -> IO ()
  throwErrnoIfMinus1Retry,
                        -- :: Num a 
                        -- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1Retry_,  
                        -- :: Num a 
                        -- =>                String -> IO a       -> IO ()
  throwErrnoIfNull,     -- ::                String -> IO (Ptr a) -> IO (Ptr a)
  throwErrnoIfNullRetry,-- ::                String -> IO (Ptr a) -> IO (Ptr a)

  throwErrnoIfRetryMayBlock, 
  throwErrnoIfRetryMayBlock_,
  throwErrnoIfMinus1RetryMayBlock,
  throwErrnoIfMinus1RetryMayBlock_,  
  throwErrnoIfNullRetryMayBlock,

  throwErrnoPath,
  throwErrnoPathIf,
  throwErrnoPathIf_,
  throwErrnoPathIfNull,
  throwErrnoPathIfMinus1,
  throwErrnoPathIfMinus1_,
) where


-- this is were we get the CONST_XXX definitions from that configure
-- calculated for us
--
#ifndef __NHC__
#include "HsBaseConfig.h"
#endif

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad            ( void )
import Data.Maybe
import GHC.Exception
import Data.Typeable
import GHC.Show
import GHC.Err

#if __GLASGOW_HASKELL__
import GHC.IO
--import GHC.IO.Exception
--import GHC.IO.Handle.Types
import GHC.Num
import GHC.Base
#elif __HUGS__
import Hugs.Prelude             ( Handle, IOError, ioError )
import System.IO.Unsafe         ( unsafePerformIO )
#else
--import System.IO                ( Handle )
--import System.IO.Error          ( IOError, ioError )
import System.IO.Unsafe         ( unsafePerformIO )
import Foreign.Storable         ( Storable(poke,peek) )
#endif

#ifdef __HUGS__
{-# CFILES cbits/PrelIOUtils.c #-}
#endif


-- "errno" type
-- ------------

-- | Haskell representation for @errno@ values.
-- The implementation is deliberately exposed, to allow users to add
-- their own definitions of 'Errno' values.

newtype Errno = Errno CInt

instance Eq Errno where
  errno1@(Errno no1) == errno2@(Errno no2) 
    | isValidErrno errno1 && isValidErrno errno2 = no1 == no2
    | otherwise                                  = False

-- common "errno" symbols
--
eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV                    :: Errno
--
-- the cCONST_XXX identifiers are cpp symbols whose value is computed by
-- configure 
--
eOK             = Errno (fromInteger 0)
#ifdef __NHC__
#include "Errno.hs"
#else
e2BIG           = Errno (fromInteger (CONST_E2BIG))
eACCES          = Errno (fromInteger (CONST_EACCES))
eADDRINUSE      = Errno (fromInteger (CONST_EADDRINUSE))
eADDRNOTAVAIL   = Errno (fromInteger (CONST_EADDRNOTAVAIL))
eADV            = Errno (fromInteger (CONST_EADV))
eAFNOSUPPORT    = Errno (fromInteger (CONST_EAFNOSUPPORT))
eAGAIN          = Errno (fromInteger (CONST_EAGAIN))
eALREADY        = Errno (fromInteger (CONST_EALREADY))
eBADF           = Errno (fromInteger (CONST_EBADF))
eBADMSG         = Errno (fromInteger (CONST_EBADMSG))
eBADRPC         = Errno (fromInteger (CONST_EBADRPC))
eBUSY           = Errno (fromInteger (CONST_EBUSY))
eCHILD          = Errno (fromInteger (CONST_ECHILD))
eCOMM           = Errno (fromInteger (CONST_ECOMM))
eCONNABORTED    = Errno (fromInteger (CONST_ECONNABORTED))
eCONNREFUSED    = Errno (fromInteger (CONST_ECONNREFUSED))
eCONNRESET      = Errno (fromInteger (CONST_ECONNRESET))
eDEADLK         = Errno (fromInteger (CONST_EDEADLK))
eDESTADDRREQ    = Errno (fromInteger (CONST_EDESTADDRREQ))
eDIRTY          = Errno (fromInteger (CONST_EDIRTY))
eDOM            = Errno (fromInteger (CONST_EDOM))
eDQUOT          = Errno (fromInteger (CONST_EDQUOT))
eEXIST          = Errno (fromInteger (CONST_EEXIST))
eFAULT          = Errno (fromInteger (CONST_EFAULT))
eFBIG           = Errno (fromInteger (CONST_EFBIG))
eFTYPE          = Errno (fromInteger (CONST_EFTYPE))
eHOSTDOWN       = Errno (fromInteger (CONST_EHOSTDOWN))
eHOSTUNREACH    = Errno (fromInteger (CONST_EHOSTUNREACH))
eIDRM           = Errno (fromInteger (CONST_EIDRM))
eILSEQ          = Errno (fromInteger (CONST_EILSEQ))
eINPROGRESS     = Errno (fromInteger (CONST_EINPROGRESS))
eINTR           = Errno (fromInteger (CONST_EINTR))
eINVAL          = Errno (fromInteger (CONST_EINVAL))
eIO             = Errno (fromInteger (CONST_EIO))
eISCONN         = Errno (fromInteger (CONST_EISCONN))
eISDIR          = Errno (fromInteger (CONST_EISDIR))
eLOOP           = Errno (fromInteger (CONST_ELOOP))
eMFILE          = Errno (fromInteger (CONST_EMFILE))
eMLINK          = Errno (fromInteger (CONST_EMLINK))
eMSGSIZE        = Errno (fromInteger (CONST_EMSGSIZE))
eMULTIHOP       = Errno (fromInteger (CONST_EMULTIHOP))
eNAMETOOLONG    = Errno (fromInteger (CONST_ENAMETOOLONG))
eNETDOWN        = Errno (fromInteger (CONST_ENETDOWN))
eNETRESET       = Errno (fromInteger (CONST_ENETRESET))
eNETUNREACH     = Errno (fromInteger (CONST_ENETUNREACH))
eNFILE          = Errno (fromInteger (CONST_ENFILE))
eNOBUFS         = Errno (fromInteger (CONST_ENOBUFS))
eNODATA         = Errno (fromInteger (CONST_ENODATA))
eNODEV          = Errno (fromInteger (CONST_ENODEV))
eNOENT          = Errno (fromInteger (CONST_ENOENT))
eNOEXEC         = Errno (fromInteger (CONST_ENOEXEC))
eNOLCK          = Errno (fromInteger (CONST_ENOLCK))
eNOLINK         = Errno (fromInteger (CONST_ENOLINK))
eNOMEM          = Errno (fromInteger (CONST_ENOMEM))
eNOMSG          = Errno (fromInteger (CONST_ENOMSG))
eNONET          = Errno (fromInteger (CONST_ENONET))
eNOPROTOOPT     = Errno (fromInteger (CONST_ENOPROTOOPT))
eNOSPC          = Errno (fromInteger (CONST_ENOSPC))
eNOSR           = Errno (fromInteger (CONST_ENOSR))
eNOSTR          = Errno (fromInteger (CONST_ENOSTR))
eNOSYS          = Errno (fromInteger (CONST_ENOSYS))
eNOTBLK         = Errno (fromInteger (CONST_ENOTBLK))
eNOTCONN        = Errno (fromInteger (CONST_ENOTCONN))
eNOTDIR         = Errno (fromInteger (CONST_ENOTDIR))
eNOTEMPTY       = Errno (fromInteger (CONST_ENOTEMPTY))
eNOTSOCK        = Errno (fromInteger (CONST_ENOTSOCK))
eNOTTY          = Errno (fromInteger (CONST_ENOTTY))
eNXIO           = Errno (fromInteger (CONST_ENXIO))
eOPNOTSUPP      = Errno (fromInteger (CONST_EOPNOTSUPP))
ePERM           = Errno (fromInteger (CONST_EPERM))
ePFNOSUPPORT    = Errno (fromInteger (CONST_EPFNOSUPPORT))
ePIPE           = Errno (fromInteger (CONST_EPIPE))
ePROCLIM        = Errno (fromInteger (CONST_EPROCLIM))
ePROCUNAVAIL    = Errno (fromInteger (CONST_EPROCUNAVAIL))
ePROGMISMATCH   = Errno (fromInteger (CONST_EPROGMISMATCH))
ePROGUNAVAIL    = Errno (fromInteger (CONST_EPROGUNAVAIL))
ePROTO          = Errno (fromInteger (CONST_EPROTO))
ePROTONOSUPPORT = Errno (fromInteger (CONST_EPROTONOSUPPORT))
ePROTOTYPE      = Errno (fromInteger (CONST_EPROTOTYPE))
eRANGE          = Errno (fromInteger (CONST_ERANGE))
eREMCHG         = Errno (fromInteger (CONST_EREMCHG))
eREMOTE         = Errno (fromInteger (CONST_EREMOTE))
eROFS           = Errno (fromInteger (CONST_EROFS))
eRPCMISMATCH    = Errno (fromInteger (CONST_ERPCMISMATCH))
eRREMOTE        = Errno (fromInteger (CONST_ERREMOTE))
eSHUTDOWN       = Errno (fromInteger (CONST_ESHUTDOWN))
eSOCKTNOSUPPORT = Errno (fromInteger (CONST_ESOCKTNOSUPPORT))
eSPIPE          = Errno (fromInteger (CONST_ESPIPE))
eSRCH           = Errno (fromInteger (CONST_ESRCH))
eSRMNT          = Errno (fromInteger (CONST_ESRMNT))
eSTALE          = Errno (fromInteger (CONST_ESTALE))
eTIME           = Errno (fromInteger (CONST_ETIME))
eTIMEDOUT       = Errno (fromInteger (CONST_ETIMEDOUT))
eTOOMANYREFS    = Errno (fromInteger (CONST_ETOOMANYREFS))
eTXTBSY         = Errno (fromInteger (CONST_ETXTBSY))
eUSERS          = Errno (fromInteger (CONST_EUSERS))
eWOULDBLOCK     = Errno (fromInteger (CONST_EWOULDBLOCK))
eXDEV           = Errno (fromInteger (CONST_EXDEV))
#endif

-- | Yield 'True' if the given 'Errno' value is valid on the system.
-- This implies that the 'Eq' instance of 'Errno' is also system dependent
-- as it is only defined for valid values of 'Errno'.
--
isValidErrno               :: Errno -> Bool
--
-- the configure script sets all invalid "errno"s to -1
--
isValidErrno (Errno errno)  = errno /= negate (fromInteger 1)


-- access to the current thread's "errno" value
-- --------------------------------------------

-- | Get the current value of @errno@ in the current thread.
--
getErrno :: IO Errno

-- We must call a C function to get the value of errno in general.  On
-- threaded systems, errno is hidden behind a C macro so that each OS
-- thread gets its own copy.
#ifdef __NHC__
getErrno = do e <- peek _errno; return (Errno e)
foreign import ccall unsafe "errno.h &errno" _errno :: Ptr CInt
#else
getErrno = do e <- get_errno; return (Errno e)
--foreign import ccall unsafe "HsBase.h __hscore_get_errno" get_errno :: IO CInt
get_errno :: IO CInt
get_errno = undefined
#endif

-- | Reset the current thread\'s @errno@ value to 'eOK'.
--
resetErrno :: IO ()

-- Again, setting errno has to be done via a C function.
#ifdef __NHC__
resetErrno = poke _errno (fromInteger 0)
#else
resetErrno = set_errno (fromInteger 0)
--foreign import ccall unsafe "HsBase.h __hscore_set_errno" set_errno :: CInt -> IO ()
set_errno :: CInt -> IO ()
set_errno = undefined
#endif

-- throw current "errno" value
-- ---------------------------

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'.
--
throwErrno     :: String        -- ^ textual description of the error location
               -> IO a
throwErrno loc  =
  do
    errno <- getErrno
    throwIO (ErrnoError loc errno Nothing)


-- guards for IO operations that may fail
-- --------------------------------------

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the result value of the 'IO' action meets the given predicate.
--
throwErrnoIf    :: (a -> Bool)  -- ^ predicate to apply to the result value
                                -- of the 'IO' operation
                -> String       -- ^ textual description of the location
                -> IO a         -- ^ the 'IO' operation to be executed
                -> IO a
throwErrnoIf pred loc f  = 
  do
    res <- f
    if pred res then throwErrno loc else return res

-- | as 'throwErrnoIf', but discards the result of the 'IO' action after
-- error handling.
--
throwErrnoIf_   :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ pred loc f  = void $ throwErrnoIf pred loc f

-- | as 'throwErrnoIf', but retry the 'IO' action when it yields the
-- error code 'eINTR' - this amounts to the standard retry loop for
-- interrupted POSIX system calls.
--
throwErrnoIfRetry            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry pred loc f  = 
  do
    res <- f
    if pred res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfRetry pred loc f
          else throwErrno loc
      else return res

-- | as 'throwErrnoIfRetry', but additionally if the operation 
-- yields the error code 'eAGAIN' or 'eWOULDBLOCK', an alternative
-- action is executed before retrying.
--
throwErrnoIfRetryMayBlock
                :: (a -> Bool)  -- ^ predicate to apply to the result value
                                -- of the 'IO' operation
                -> String       -- ^ textual description of the location
                -> IO a         -- ^ the 'IO' operation to be executed
                -> IO b         -- ^ action to execute before retrying if
                                -- an immediate retry would block
                -> IO a
throwErrnoIfRetryMayBlock pred loc f on_block  = 
  do
    res <- f
    if pred res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfRetryMayBlock pred loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
                 then do _ <- on_block
                         throwErrnoIfRetryMayBlock pred loc f on_block
                 else throwErrno loc
      else return res

-- | as 'throwErrnoIfRetry', but discards the result.
--
throwErrnoIfRetry_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ pred loc f  = void $ throwErrnoIfRetry pred loc f

-- | as 'throwErrnoIfRetryMayBlock', but discards the result.
--
throwErrnoIfRetryMayBlock_ :: (a -> Bool) -> String -> IO a -> IO b -> IO ()
throwErrnoIfRetryMayBlock_ pred loc f on_block 
  = void $ throwErrnoIfRetryMayBlock pred loc f on_block

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns a result of @-1@.
--
throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1  = throwErrnoIf (== negate (fromInteger 1))

-- | as 'throwErrnoIfMinus1', but discards the result.
--
throwErrnoIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1_  = throwErrnoIf_ (== negate (fromInteger 1))

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns a result of @-1@, but retries in case of
-- an interrupted operation.
--
throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== negate (fromInteger 1))

-- | as 'throwErrnoIfMinus1', but discards the result.
--
throwErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_  = throwErrnoIfRetry_ (== negate (fromInteger 1))

-- | as 'throwErrnoIfMinus1Retry', but checks for operations that would block.
--
throwErrnoIfMinus1RetryMayBlock :: (Eq a, Num a)
                                => String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock  = throwErrnoIfRetryMayBlock (== negate (fromInteger 1))

-- | as 'throwErrnoIfMinus1RetryMayBlock', but discards the result.
--
throwErrnoIfMinus1RetryMayBlock_ :: (Eq a, Num a)
                                 => String -> IO a -> IO b -> IO ()
throwErrnoIfMinus1RetryMayBlock_  = throwErrnoIfRetryMayBlock_ (== negate (fromInteger 1))

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns 'nullPtr'.
--
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrnoIf (== nullPtr)

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns 'nullPtr',
-- but retry in case of an interrupted operation.
--
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry  = throwErrnoIfRetry (== nullPtr)

-- | as 'throwErrnoIfNullRetry', but checks for operations that would block.
--
throwErrnoIfNullRetryMayBlock :: String -> IO (Ptr a) -> IO b -> IO (Ptr a)
throwErrnoIfNullRetryMayBlock  = throwErrnoIfRetryMayBlock (== nullPtr)

-- | as 'throwErrno', but exceptions include the given path when appropriate.
--
throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    throwIO (ErrnoError loc errno (Just path))

-- | as 'throwErrnoIf', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf pred loc path f =
  do
    res <- f
    if pred res then throwErrnoPath loc path else return res

-- | as 'throwErrnoIf_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ pred loc path f  = void $ throwErrnoPathIf pred loc path f

-- | as 'throwErrnoIfNull', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull  = throwErrnoPathIf (== nullPtr)

-- | as 'throwErrnoIfMinus1', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== negate (fromInteger 1))

-- | as 'throwErrnoIfMinus1_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== negate (fromInteger 1))

-- conversion of an "errno" value into IO error
-- --------------------------------------------

data ErrnoError = ErrnoError String Errno (Maybe String)

instance Typeable ErrnoError
instance Show ErrnoError
instance Exception ErrnoError

