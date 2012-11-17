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

{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}

module Network.NBD.Server (
      negotiateNewstyle
    , sendExportInformation
    , getCommand
    , sendReply
    , sendReplyData
    , sendError
    ) where

import Data.Bits
import Data.Word
import Data.Serialize

import Data.Conduit
import Data.Conduit.Binary as CB

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Control.Exception.Base (throwIO)

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Error (Errno(Errno))
import Foreign.Storable (Storable, sizeOf)

import Network.NBD.Types
import Network.NBD.Constants

negotiateNewstyle :: MonadIO m => Maybe [ExportName]
                               -> Pipe BS.ByteString BS.ByteString BS.ByteString () m ExportName
negotiateNewstyle exports = do
    yield newstylePrelude

    clientFlags <- recvWord32
    when (clientFlags /= 0 && clientFlags /= 1) $
        liftIO $ throwIO $ InvalidClientFlags clientFlags

    handleOptions
  where
    handleOptions = do
        magic <- recvWord64
        when (magic /= nBD_OPTS_MAGIC) $
            liftIO $ throwIO $ InvalidMagic "option" magic

        cmd <- recvWord32
        len <- recvWord32
        dat <- if len /= 0
                   then CB.take $ fromIntegral len
                   else return LBS.empty

        case toEnum $ fromIntegral cmd of
            UnknownOption -> do
                sendOptionReply cmd ErrorUnsupported BS.empty
                handleOptions
            Abort -> handleAbort
            List -> do
                if len /= 0
                    then sendOptionReply cmd ErrorInvalid BS.empty
                    else handleList cmd
                handleOptions
                    
            ExportName -> return $ decodeUtf8 $ BS.concat $ LBS.toChunks dat

    handleAbort = liftIO $ throwIO ClientAbort
    {-# INLINE handleAbort #-}

    handleList cmd = case exports of
        Nothing -> sendOptionReply cmd ErrorPolicy BS.empty
        Just l -> do
            forM_ l $ \e -> do
                let p = runPut $ do
                    let bs = encodeUtf8 e
                    putWord32be $ fromIntegral $ BS.length bs
                    putByteString bs
                sendOptionReply cmd Server p
            sendOptionReply cmd Ack BS.empty

    sendOptionReply cmd typ dat =
        yield $ runPut $ do
            putWord64be nBD_REP_MAGIC
            putWord32be cmd
            putWord32be $ fromIntegral $ fromEnum typ
            putWord32be $ fromIntegral $ BS.length dat
            putByteString dat

    newstylePrelude :: BS.ByteString
    newstylePrelude = runPut $ do
        putByteString nBD_INIT_PASSWD
        putWord64be nBD_OPTS_MAGIC
        putWord16be $ fromIntegral smallflags
      where
        smallflags = 0 .|. fromEnum ServerFixedNewstyle
        {-# INLINE smallflags #-}
    {-# INLINE newstylePrelude #-}

sendExportInformation :: Monad m => ExportSize
                                 -> [NbdExportFlag]
                                 -> Pipe l i BS.ByteString u m ()
sendExportInformation len flags = do
    sendWord64 len
    sendWord16 flags'
    yield zeros
  where
    zeros = BS.replicate 124 0
    flags' = foldr (\f a -> a .|. fromIntegral (fromEnum f)) 0 flags

recvValue :: (Serialize r, Storable r, MonadIO m) => Get r
                                                  -> r
                                                  -> Pipe BS.ByteString BS.ByteString o u m r
recvValue parser r = do
    bytes <- CB.take $ sizeOf r
    case runGetLazy parser bytes of
        Right n -> return n
        Left s -> liftIO $ throwIO $ ParseFailure s
{-# INLINE recvValue #-}

recvWord32 :: MonadIO m => Pipe BS.ByteString BS.ByteString o u m Word32
recvWord32 = recvValue getWord32be (undefined :: Word32)
{-# INLINE recvWord32 #-}
recvWord64 :: MonadIO m => Pipe BS.ByteString BS.ByteString o u m Word64
recvWord64 = recvValue getWord64be (undefined :: Word64)
{-# INLINE recvWord64 #-}

sendValue :: Monad m => (a -> Put)
                     -> a
                     -> Pipe l i BS.ByteString u m ()
sendValue putter = yield . runPut . putter
{-# INLINE sendValue #-}
sendWord16 :: Monad m => Word16
                      -> Pipe l i BS.ByteString u m ()
sendWord16 = sendValue putWord16be
{-# INLINE sendWord16 #-}
sendWord64 :: Monad m => Word64
                      -> Pipe l i BS.ByteString u m ()
sendWord64 = sendValue putWord64be
{-# INLINE sendWord64 #-}

getCommand :: MonadIO m => Pipe BS.ByteString BS.ByteString o u m Command
getCommand = do
    -- TODO Replace these recv's with a single Serialize action which pulls
    -- larger chunks from the source
    -- I.e. create an updated version of cereal-conduit and use it
    -- accordingly.
    !magic <- recvWord32
    when (magic /= nBD_REQUEST_MAGIC) $
        liftIO $ throwIO $ InvalidMagic "request" (fromIntegral magic)

    !typ <- recvWord32
    !handle <- Handle `fmap` recvWord64
    !offset <- recvWord64
    !len <- recvWord32

    let cmd = typ .&. nBD_CMD_MASK_COMMAND
        -- TODO Remove hard-coded list
        flags = filter (\v -> (typ .&. fromIntegral (fromEnum v)) /= 0) [ForceUnitAccess]

    case cmd of
        0 -> return $ Read handle offset len flags
        1 -> do
            dat <- if len /= 0
                       then CB.take $ fromIntegral len
                       else return LBS.empty
            return $ Write handle offset dat flags
        2 -> return $ Disconnect flags
        3 -> return $ Network.NBD.Types.Flush handle flags
        4 -> return $ Trim handle offset len flags
        _ -> return $ UnknownCommand cmd handle offset len flags

sendReply :: Monad m => Handle
                     -> Pipe l i BS.ByteString u m ()
sendReply h =
    yield $ runPut $ do
        putWord32be nBD_REPLY_MAGIC
        putWord32be 0
        put h
{-# INLINE sendReply #-}

sendReplyData :: Monad m => Handle
                         -> LBS.ByteString
                         -> Pipe l i BS.ByteString u m ()
sendReplyData h d =
    yield $ runPut $ do
        putWord32be nBD_REPLY_MAGIC
        putWord32be 0
        put h
        putLazyByteString d
{-# INLINE sendReplyData #-}

sendError :: Monad m => Handle
                     -> Errno
                     -> Pipe l i BS.ByteString u m ()
sendError h (Errno e) =
    yield $ runPut $ do
        putWord32be nBD_REPLY_MAGIC
        putWord32be $ fromIntegral e
        put h