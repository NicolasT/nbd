{- nbd - A Haskell library to implement NBD servers
 -
 - Copyright (C) 2012  Nicolas Trangez
 -
 - This library is free software; you can redistribute it and/or
 - modify it under the terms of the GNU Lesser General Public
 - License as published by the Free Software Foundation; either
 - version 2.1 of the License, or (at your option) any later version.
 -
 - This library is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 - Lesser General Public License for more details.
 -
 - You should have received a copy of the GNU Lesser General Public
 - License along with this library; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module System.Posix.IO.Extra (
      fsync
    , fdatasync
    , pwrite
    , pwriteAll
    , pwriteAllLazy

    , fALLOC_FL_KEEP_SIZE
    , fALLOC_FL_PUNCH_HOLE
    , fallocate
    ) where

#define _GNU_SRC
#include <fcntl.h>
#include <linux/falloc.h>

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import System.Posix.Types (Fd(Fd), CSsize(..), COff(..))

import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfMinus1_, throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_)
import Foreign.C.Types (CInt(..), CChar(..), CSize(..))

foreign import ccall unsafe "unistd.h fsync"
    c_fsync :: CInt -> IO CInt

fsync :: Fd -> IO ()
fsync (Fd fd) = throwErrnoIfMinus1_ "fsync" $ c_fsync fd

foreign import ccall unsafe "unistd.h fdatasync"
    c_fdatasync :: CInt -> IO CInt

fdatasync :: Fd -> IO ()
fdatasync (Fd fd) = throwErrnoIfMinus1_ "fdatasync" $ c_fdatasync fd

foreign import ccall unsafe "unistd.h pwrite"
    c_pwrite :: CInt -> Ptr CChar -> CSize -> COff -> IO CSsize

pwrite :: Fd -> BS.ByteString -> COff -> IO CSsize
pwrite (Fd fd) bs off = BS.useAsCStringLen bs $ \(ptr, len) ->
    throwErrnoIfMinus1Retry "pwrite" $ c_pwrite fd ptr (fromIntegral len) off

pwriteAll :: Fd -> BS.ByteString -> COff -> IO ()
pwriteAll fd bs off = loop 0 (fromIntegral $ BS.length bs)
  where
    loop done todo
        | todo == 0 = return ()
        | otherwise = do
            l <- pwrite fd (BS.drop done bs) (off + fromIntegral done)
            loop (done + fromIntegral l) (todo - l)

pwriteAllLazy :: Fd -> LBS.ByteString -> COff -> IO ()
pwriteAllLazy fd bs = loop (LBS.toChunks bs)
  where
    loop chunks off = case chunks of
        [] -> return ()
        (x:xs) -> pwriteAll fd x off >> loop xs (off + fromIntegral (BS.length x))


foreign import ccall unsafe "fcntl.h fallocate"
    c_fallocate :: CInt -> CInt -> COff -> COff -> IO CInt

fALLOC_FL_KEEP_SIZE :: CInt
fALLOC_FL_KEEP_SIZE = #const FALLOC_FL_KEEP_SIZE

fALLOC_FL_PUNCH_HOLE :: CInt
fALLOC_FL_PUNCH_HOLE = #const FALLOC_FL_PUNCH_HOLE

fallocate :: Fd -> CInt -> COff -> COff -> IO ()
fallocate (Fd fd) mode offset len =
    throwErrnoIfMinus1Retry_ "fallocate" $ c_fallocate fd mode offset len
