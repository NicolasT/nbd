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

{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, Rank2Types #-}

module Network.NBD.Server (
      negotiateNewstyle
    , sendExportInformation
    , getCommand
    , sendReply
    , sendReplyData
    , sendError

    , Handle
    , Command(..)
    ) where

import Data.Bits
import Data.Word
import Data.Serialize

import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Cereal

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Control.Exception.Base (throwIO)

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Error (Errno(Errno))

import Network.NBD.Types
import Network.NBD.Constants

-- | Run the new-style protocol negotiation handshake with a client
-- The result will be the name of the export the client wants to connect
-- to.
negotiateNewstyle :: MonadIO m => Maybe [ExportName]  -- ^ An optional list of export names. If 'Nothing', export listing support will be disabled.
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
        sourcePut $ do
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

-- | After negotiation, send information about the selected export
sendExportInformation :: Monad m => ExportSize      -- ^ Size of the selected export
                                 -> [NbdExportFlag] -- ^ Flags set for the export
                                 -> Pipe l i BS.ByteString u m ()
sendExportInformation len flags = do
    sendWord64 len
    sendWord16 flags'
    yield zeros
  where
    zeros = BS.replicate 124 0
    flags' = foldr (\f a -> a .|. fromIntegral (fromEnum f)) 0 flags


sinkGet' :: MonadIO m => Get a -> GLSink BS.ByteString m a
sinkGet' g = do
    r <- sinkGet g
    case r of
        Right v -> return v
        Left s -> liftIO $ throwIO $ ParseFailure s
{-# INLINE sinkGet' #-}

recvWord32 :: MonadIO m => GLSink BS.ByteString m Word32
recvWord32 = sinkGet' getWord32be
{-# INLINE recvWord32 #-}
recvWord64 :: MonadIO m => GLSink BS.ByteString m Word64
recvWord64 = sinkGet' getWord64be
{-# INLINE recvWord64 #-}

sendWord16 :: Monad m => Word16
                      -> Pipe l i BS.ByteString u m ()
sendWord16 = sourcePut . putWord16be
{-# INLINE sendWord16 #-}
sendWord64 :: Monad m => Word64
                      -> Pipe l i BS.ByteString u m ()
sendWord64 = sourcePut . putWord64be
{-# INLINE sendWord64 #-}

-- | Receive a single command from the client
getCommand :: MonadIO m => Pipe BS.ByteString BS.ByteString o u m Command
getCommand = do
    -- TODO Replace these recv's with a single Serialize action which pulls
    -- larger chunks from the source
    -- I.e. create an updated version of cereal-conduit and use it
    -- accordingly.
    !magic <- recvWord32
    when (magic /= nBD_REQUEST_MAGIC) $
        liftIO $ throwIO $ InvalidMagic "request" (fromIntegral magic)

    (typ, handle, offset, len) <- sinkGet' $ do
        !typ <- getWord32be
        !handle <- Handle `fmap` get
        !offset <- getWord64be
        !len <- getWord32be
        return (typ, handle, offset, len)

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

-- | Send a reply to the client indicating success for the given command handle
sendReply :: Monad m => Handle
                     -> Pipe l i BS.ByteString u m ()
sendReply h = sendReplyData h LBS.empty
{-# INLINE sendReply #-}

-- | Send a reply to the client indicating success for the given command handle.
-- The provided data will be passed along (e.g. in response to
-- a 'Network.NBD.Types.Read' command).
sendReplyData :: Monad m => Handle
                         -> LBS.ByteString
                         -> Pipe l i BS.ByteString u m ()
sendReplyData h d =
    -- I'd love to use sourcePutLazy here, but that ruins performance
    sourcePut $ do
        putWord32be nBD_REPLY_MAGIC
        putWord32be 0
        put h
        putLazyByteString d
{-# INLINE sendReplyData #-}

-- | Send an error reply to the client for the given handle
sendError :: Monad m => Handle
                     -> Errno
                     -> Pipe l i BS.ByteString u m ()
sendError h (Errno e) =
    sourcePut $ do
        putWord32be nBD_REPLY_MAGIC
        putWord32be $ fromIntegral e
        put h
