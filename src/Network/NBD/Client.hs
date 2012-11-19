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

{-# LANGUAGE BangPatterns, RankNTypes #-}
module Network.NBD.Client (
      negotiateNewstyle

    , optionAbort
    , optionList
    , optionExportName

    , read
    , write
    , disconnect
    , flush
    , trim

    , getResponse
    , Response(..)

    , handle0
    , newHandle
    ) where

import Prelude hiding (read)

import Data.Bits
import Data.Word
import Data.Serialize hiding (flush)

import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Conduit
import Data.Conduit.Cereal

import Control.Exception.Base (throwIO)

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Foreign.C.Error (Errno(Errno))

import Network.NBD.Types
import Network.NBD.Utils
import Network.NBD.Constants

fromEnum' :: (Enum a, Num b) => a -> b
fromEnum' = fromIntegral . fromEnum
{-# INLINE fromEnum' #-}

-- | Run a newstyle negotiation handshake with the server
negotiateNewstyle :: MonadIO m => GLConduit BS.ByteString m BS.ByteString
negotiateNewstyle = do
    (initPasswd, optsMagic, smallflags) <- sinkGet' $ do
        !initPasswd <- getBytes (BS.length nBD_INIT_PASSWD)
        !optsMagic <- getWord64be
        !smallflags <- getWord16be
        return (initPasswd, optsMagic, smallflags)

    when (initPasswd /= nBD_INIT_PASSWD) $
        liftIO $ throwIO $ InvalidProtocolHeader (BS.copy initPasswd)

    when (optsMagic /= nBD_OPTS_MAGIC) $
        liftIO $ throwIO $ InvalidMagic "negotiate" optsMagic

    when (smallflags .&. fromEnum' ServerFixedNewstyle == 0) $
        liftIO $ throwIO $ InvalidServerFlags smallflags

    sourcePut $
        putWord32be (0 .|. fromEnum' ClientFixedNewstyle)


sendOption :: Monad m => NbdOption -> BS.ByteString -> GSource m BS.ByteString
sendOption option dat =
    sourcePut $ do
        putWord64be nBD_OPTS_MAGIC
        putWord32be $ fromEnum' option

        let len = fromIntegral $ BS.length dat
        putWord32be len

        when (len /= 0) $
            putByteString dat

-- | Send the Abort option to the server
optionAbort :: Monad m => GSource m BS.ByteString
optionAbort = sendOption Abort BS.empty

-- | Send the List option to the server. If the server returns a list of
-- export names, this will be returned. If this is disabled (by policy),
-- 'Nothing' is returned.
optionList :: MonadIO m => forall u. Pipe BS.ByteString BS.ByteString BS.ByteString u m (Maybe [ExportName])
optionList = do
    sendOption List BS.empty
    loop []
  where
    loop acc = do
        (magic, cmd, typ, dat) <- sinkGet' $ do
            !magic <- getWord64be
            !cmd <- getWord32be
            !typ <- getWord32be
            !len <- getWord32be
            dat <- getBytes $ fromIntegral len
            return (magic, cmd, typ, dat)

        when (magic /= nBD_REP_MAGIC) $
            liftIO $ throwIO $ InvalidMagic "option" magic
        when (cmd /= fromEnum' List) $
            liftIO $ throwIO $ UnexpectedServerReply $ "Unexpected option: " ++ show cmd

        handle typ dat acc

    handle typ dat acc
        | typ == fromEnum' Ack = return $ Just acc
        | typ == fromEnum' Server = do
            let n = flip runGet dat $ do
                len <- getWord32be
                getBytes $ fromIntegral len
            case n of
                Left s -> liftIO $ throwIO $ ParseFailure s
                Right n' -> loop $ decodeUtf8 n' : acc
        | typ == fromEnum' ErrorPolicy = return Nothing
        | otherwise = liftIO $ throwIO $ OptionError typ

-- | Send the ExportName option to the server
optionExportName :: MonadIO m => ExportName -> Pipe BS.ByteString BS.ByteString BS.ByteString u m (ExportSize, [NbdExportFlag])
optionExportName name = do
    sendOption ExportName (encodeUtf8 name)

    (len, flags, zeros) <- sinkGet' $ do
        !len <- getWord64be
        !flags <- getWord16be
        zeros <- getBytes 124
        return (len, flags, zeros)

    when (flags .&. fromEnum' HasFlags == 0) $
        liftIO $ throwIO $ UnexpectedServerReply "HasFlags not set"
    when (zeros /= expectedZeros) $
        liftIO $ throwIO $ UnexpectedServerReply "No 124 zeros in export info"

    -- TODO Remove hard-coded list
    let flags' = filter
                    (\f -> flags .&. fromEnum' f /= 0)
                    [HasFlags, ReadOnly, SendFlush, SendFua, Rotational, SendTrim]

    return (len, flags')

  where
    expectedZeros = BS.replicate 124 0


sendCommand :: Monad m => Word32 -> [NbdCommandFlag] -> Handle -> Offset -> Length -> LBS.ByteString -> GSource m BS.ByteString
sendCommand command flags handle offset len dat = sourcePut $ do
    putWord32be nBD_REQUEST_MAGIC
    putWord32be command'
    put handle
    putWord64be offset
    putWord32be len
    putLazyByteString dat
  where
    command' = foldr (\f a -> a .|. fromEnum' f) command flags

-- | Send a Read command to the server
read :: Monad m => Handle -> [NbdCommandFlag] -> Offset -> Length -> GSource m BS.ByteString
read handle flags offset len = sendCommand 0 flags handle offset len LBS.empty

-- | Send a Write command to the server
write :: Monad m => Handle -> [NbdCommandFlag] -> Offset -> Length -> LBS.ByteString -> GSource m BS.ByteString
write handle flags = sendCommand 1 flags handle

-- | Send a Disconnect command to the server
disconnect :: Monad m => Handle -> [NbdCommandFlag] -> GSource m BS.ByteString
disconnect handle flags = sendCommand 2 flags handle 0 0 LBS.empty

-- | Send a Flush command to the server
flush :: Monad m => Handle -> [NbdCommandFlag] -> GSource m BS.ByteString
flush handle flags = sendCommand 3 flags handle 0 0 LBS.empty

-- | Send a Trim command to the server
trim :: Monad m => Handle -> [NbdCommandFlag] -> Offset -> Length -> GSource m BS.ByteString
trim handle flags offset len = sendCommand 4 flags handle offset len LBS.empty

-- | Retrieve a single Response from the server. The passed function should
-- return the expected payload size (in case of a Read request), or
-- Nothing, based on the handle.
getResponse :: MonadIO m => (Handle -> Maybe Length) -> GLSink BS.ByteString m Response
getResponse getSize = do
    magic <- sinkGet' getWord32be
    when (magic /= nBD_REPLY_MAGIC) $
        liftIO $ throwIO $ InvalidMagic "response" $ fromIntegral magic

    sinkGet' $ do
        errno <- getWord32be
        handle <- get

        case errno of
            0 -> do
                let len = getSize handle
                case len of
                    Nothing -> return $ Success handle
                    Just len' -> case len' of
                                 0 -> return $ Data handle LBS.empty
                                 len'' -> Data handle `fmap` getLazyByteString (fromIntegral len'')
            n -> return $ Error handle $ Errno $ fromIntegral n
