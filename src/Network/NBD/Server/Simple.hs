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

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Network.NBD.Server.Simple (
    -- * Action result representations
      Response(..)
    , ReadResponse(..)
    , FileRange(..)

    -- * Action handlers
    , ExportHandler(..)
    , Offset, Length
    , NbdCommandFlag(..)
    , Flags
    , ReadHandler
    , WriteHandler
    , FlushHandler
    , TrimHandler
    , DisconnectHandler
    , CommandId, UnknownCommandHandler

    -- * Server execution
    , ExportSize
    , ExportMaker
    , ServerSettings(..)
    , HostPreference(..)
    , nBD_DEFAULT_PORT
    , runServer

    -- * Utilities
    , withBoundsCheck
    ) where

import Control.Exception (catch)
import Control.Exception.Base (Exception, SomeException, throwIO, try)
import GHC.IO.Exception (ioe_errno)

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Maybe (isJust, isNothing)

import Data.Conduit hiding (Flush)
import Data.Conduit.Network hiding (ServerSettings)
import Data.Conduit.Network.Basic (ServerSettings(..))
import qualified Data.Conduit.Network.Basic as NB

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Typeable (Typeable)

import Data.Word (Word32)

import Network.Socket (Socket)
import qualified Network.Socket as NS

import Network.Sendfile (FileRange(..), sendfileFdWithHeader)

import Foreign.C.Error (Errno(Errno), eINVAL, eOPNOTSUPP, getErrno)

import System.Posix.Types (Fd)

import Network.NBD (ExportName, ExportSize, Handle, Length, Offset, NbdCommandFlag(..), NbdExportFlag(..), nBD_DEFAULT_PORT)
import Network.NBD.Server

data NbdAppData m = NbdAppData { nbdAppSource :: Source m BS.ByteString
                               , nbdAppSink :: Sink BS.ByteString m ()
                               , nbdAppSockAddr :: NS.SockAddr
                               , nbdAppSocket :: Socket
                               }

-- | Response data returned by a 'ReadHandler'
data ReadResponse = Data LBS.ByteString   -- ^ Return given data as request reply
                  | Sendfile Fd FileRange -- ^ Use sendfile to return some data

-- | Success indicator returned by action handlers
data Response a = OK a        -- ^ Success
                | Error Errno -- ^ Failure. The given 'Errno' will be returned to the client

-- | Command identifier for requests received from a client
type CommandId = Word32
-- | Type synonym for a list of 'NbdCommandFlag's
type Flags = [NbdCommandFlag]

-- | 'Read' request handler prototype
type ReadHandler = Offset -> Length -> Flags -> IO (Response ReadResponse)
-- | 'Write' request handler prototype
type WriteHandler = Offset -> LBS.ByteString -> Flags -> IO (Response ())
-- | 'Flush' request handler prototype
type FlushHandler = Flags -> IO (Response ())
-- | 'Trim' request handler prototype
type TrimHandler = Offset -> Length -> Flags -> IO (Response ())
-- | Disconnect request handler prototype. Note the connection will be
-- closed by the server when receiving a 'Disconnect' request, after
-- executing any given handler, even if the handler throws an 'Exception'.
type DisconnectHandler = Flags -> IO ()
-- | Unknown request handler prototype
type UnknownCommandHandler = CommandId -> Offset -> Length -> Flags -> IO (Response ())

-- | Action vtable to handle client requests. Any 'Maybe' field can be set
-- to 'Nothing', which implies the given feature is not available on the
-- server. The feature flags sent to a client during protocol negotiation
-- will be based on this.
data ExportHandler = ExportHandler { handleRead :: ReadHandler -- ^ Handle a 'Read' request
                                   , handleWrite :: Maybe WriteHandler -- ^ Handle a 'Write' request
                                   , handleFlush :: Maybe FlushHandler -- ^ Handle a 'Flush' request
                                   , handleTrim :: Maybe TrimHandler -- ^ Handle a 'Trim' request
                                   , handleDisconnect :: Maybe DisconnectHandler -- ^ Action to run prior to disconnecting a client due to a 'Disconnect' request
                                   , handleUnknownCommand :: Maybe UnknownCommandHandler -- ^ Handle an unknown request
                                   , handleSize :: ExportSize -- ^ Size of the export
                                   , handleHasFUA :: Bool -- ^ Server supports the 'ForceUnitAccess' request flag
                                   , handleIsRotational :: Bool -- ^ Server exposes a rotational device (which might impact the client IO scheduler)
                                   }

-- | Prototype of an action turning a 'SockAddr' and 'ExportName' into an
-- export, or rather its 'ExportHandler' vtable. If 'Nothing' is returned,
-- this implies the given 'ExportName' was invalid, and the server should
-- act accordingly.
type ExportMaker = NS.SockAddr -> ExportName -> IO (Maybe ExportHandler)

-- | Exceptions generated by the server
data ServerError = InvalidExport ExportName -- ^ Client requested an unknown or invalid export
  deriving (Show, Typeable)
instance Exception ServerError

data Forever a = Loop
               | Return a

forever' :: Monad m => m (Forever a) -> m a
forever' act = loop Loop
  where
    loop Loop = act >>= loop
    loop (Return a) = return a

application :: Maybe [ExportName]
            -> ExportMaker
            -> NbdAppData IO
            -> IO ()
application exportNames exportMaker client =
    nbdAppSource client $= handler $$ nbdAppSink client
  where
    handler = do
        target <- negotiateNewstyle exportNames

        maybeHandler <- lift $ exportMaker (nbdAppSockAddr client) target
        case maybeHandler of
            Nothing -> liftIO $ throwIO $ InvalidExport target
            Just handler' -> do
                sendExportInformation (handleSize handler') (calculateFlags handler')
                loop handler'

    calculateFlags :: ExportHandler -> [NbdExportFlag]
    calculateFlags handler' = HasFlags : map snd (filter fst flags)
      where
        flags = [ (isNothing $ handleWrite handler', ReadOnly)
                , (isJust $ handleFlush handler', SendFlush)
                , (handleHasFUA handler', SendFua)
                , (handleIsRotational handler', Rotational)
                , (isJust $ handleTrim handler', SendTrim)
                ]

    loop = forever' . loop'

    loop' handler' = do
        req <- getCommand
        case req of
            Read h o l f -> do
                handleErrno h
                    (maybe (return ()) (\v -> case v of
                        Error e -> sendError h e
                        OK d -> sendReplyData h d))
                    (do
                        val <- handleRead handler' o l f
                        case val of
                            Error e -> return $ Just $ Error e
                            OK (Data d) -> return $ Just $ OK d
                            OK (Sendfile fd r) -> do
                                sendReplyData' h (\header ->
                                    liftIO $ sendfileFdWithHeader (nbdAppSocket client) fd r (return ()) [header])
                                return Nothing)
                return Loop
            Write h o d f -> do
                ifSupported h handler' handleWrite $ \act ->
                    handleErrno h
                        (handleGenericResponse h)
                        (act o d f)
                return Loop
            Disconnect f -> case handleDisconnect handler' of
                            Nothing -> return $ Return ()
                            Just g -> liftIO $ Control.Exception.catch
                                (liftIO (g f) >> return (Return ()))
                                (\(_ :: SomeException) -> return $ Return ())
            Flush h f -> do
                ifSupported h handler' handleFlush $ \act ->
                    handleErrno h
                        (handleGenericResponse h)
                        (act f)
                return Loop
            Trim h o l f -> do
                ifSupported h handler' handleTrim $ \act ->
                    handleErrno h
                        (handleGenericResponse h)
                        (act o l f)
                return Loop
            UnknownCommand i h o l f -> do
                ifSupported h handler' handleUnknownCommand $ \act ->
                    handleErrno h
                        (handleGenericResponse h)
                        (act i o l f)
                return Loop

    ifSupported h handler' f act =
        case f handler' of
            Nothing -> sendError h eOPNOTSUPP
            Just g -> act g

    handleErrno :: MonadIO m
                => Handle
                -> (a -> Pipe l i BS.ByteString u m ())
                -> IO a
                -> Pipe l i BS.ByteString u m ()
    handleErrno h act go = do
        res <- liftIO $ try go
        case res of
            Left (exc :: IOError) ->
                case ioe_errno exc of
                    Nothing -> liftIO getErrno >>= sendError h
                    Just e -> sendError h (Errno e)
            Right val -> act val

    handleGenericResponse :: Monad m
                          => Handle
                          -> Response ()
                          -> Pipe l i BS.ByteString u m ()
    handleGenericResponse h resp = case resp of
        OK () -> sendReply h
        Error e -> sendError h e


makeServer :: Monad m
           => Maybe [ExportName]
           -> ExportMaker
           -> (Socket, NS.SockAddr)
           -> m (IO ())
makeServer exportNames exportMaker (sock, addr) = return $
    application exportNames exportMaker NbdAppData { nbdAppSource = sourceSocket sock
                                                   , nbdAppSink = sinkSocket sock
                                                   , nbdAppSockAddr = addr
                                                   , nbdAppSocket = sock
                                                   }

-- | Run an NBD server
runServer :: ServerSettings -- ^ Listen address
          -> Maybe [ExportName] -- ^ Optional list of export names to expose to the client. If 'Nothing', export listing is disabled.
          -> ExportMaker -- ^ Action to retrieve an 'ExportHandler' for a given client connection
          -> IO ()
runServer settings exportNames exportMaker =
    NB.runServer settings $ makeServer exportNames exportMaker


-- | Utility action to validate request boundaries. This action will return
-- 'eINVAL' to the client if an out-of-bounds request was received
withBoundsCheck :: Monad m
                => ExportSize -- ^ Size of the export we're dealing with
                -> Offset -- ^ Offset of the request
                -> Length -- ^ Length of the request
                -> m (Response a) -- ^ Action to execute if request is within bounds
                -> m (Response a)
withBoundsCheck size o l act =
    if (o >= maxBound - fromIntegral l) || (o + fromIntegral l > size)
        then return (Error eINVAL)
        else act
