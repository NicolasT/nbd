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

{-# LANGUAGE FlexibleContexts #-}

module Data.Conduit.Network.Basic (
      Application
    , ApplicationBuilder
    , ServerSettings(..)
    , runServer
    ) where

import Control.Concurrent (forkIO)

import Control.Exception.Base (bracket, bracketOnError, finally)

import Control.Monad (forever, void)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.Control (control)
import Control.Monad.Trans.Resource (MonadBaseControl)

import Network.Socket (Socket)
import qualified Network.Socket as NS

import Data.Conduit.Network hiding (Application, ServerSettings)

data ServerSettings = TCPServerSettings Int HostPreference
                    | UNIXServerSettings FilePath

type Application m = m ()
type ApplicationBuilder m = ((Socket, NS.SockAddr) -> m (Application m))

runServer :: (MonadBaseControl IO m, MonadIO m) => ServerSettings
                                                -> ApplicationBuilder m
                                                -> m ()
runServer settings mkApp = control $ \run -> bracket
    (liftIO doBind)
    (liftIO . NS.sClose)
    (run . forever . serve)
  where
    serve lsocket = do
        s@(socket, _) <- liftIO $ acceptSafe lsocket
        app <- mkApp s
        let app' run = void $ run app
            appClose run = app' run `finally` NS.sClose socket
        control $ \run -> forkIO (appClose run) >> run (return ())

    doBind = case settings of
        TCPServerSettings port host -> bindPort port host
        UNIXServerSettings path -> bindUNIX path

    bindUNIX path = do
        sock <- bracketOnError
            (NS.socket NS.AF_UNIX NS.Stream 0)
            NS.close
            (\sock -> do
                NS.bindSocket sock (NS.SockAddrUnix path)
                return sock
            )
        NS.listen sock (max 2048 NS.maxListenQueue)
        return sock
