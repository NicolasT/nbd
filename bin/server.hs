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
module Main (
      main
    ) where

import Data.Conduit hiding (Flush)
import Data.Conduit.Network

import qualified Data.Map as Map

import Control.Monad.Trans.Resource

import qualified Data.Text as Text

import Data.Typeable (Typeable)

import Control.Concurrent (threadDelay)

import Control.Exception.Base (Exception, throwIO, try)
import GHC.IO.Exception (ioe_errno)

import Control.Monad (forever, forM, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Error (Errno(Errno), eINVAL, eOPNOTSUPP, ePERM, getErrno)

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)

import System.Posix.Files (fileSize, getFdStatus)
import System.Posix.IO (closeFd, defaultFileFlags, openFd)
import qualified System.Posix.IO as IO
import System.Posix.Types (Fd)
import System.Posix.IO.ByteString.Lazy (fdPread)

import Network.NBD as N
import Network.NBD.Server as S

data Export = Export { exportHandle :: Fd
                     , exportSize :: N.ExportSize
                     }
  deriving (Show)

data ServerError = InvalidExport N.ExportName
  deriving (Show, Typeable)
instance Exception ServerError

application :: MonadIO m => Map.Map N.ExportName Export -> AppData m -> m ()
application exports dat = appSource dat $= handler $$ appSink dat
  where
    handler = do
        let exportNames = Just $ Map.keys exports
        target <- S.negotiateNewstyle exportNames

        when (Map.notMember target exports) $
            liftIO $ throwIO $ InvalidExport target

        let export = (Map.!) exports target

        S.sendExportInformation (exportSize export) flags

        loop (exportHandle export) (exportSize export)

    loop handle size = loop'
      where
        loop' = do
            req <- S.getCommand
            case req of
                Read h o l _ -> {-# SCC "handleRead" #-} do
                    if (o > maxBound - fromIntegral l) || (o + fromIntegral l > size)
                        then sendError h eINVAL
                        else do
                            res <- liftIO $ try $ fdPread handle (fromIntegral l) (fromIntegral o)
                            case res of
                                Left (exc :: IOError) ->
                                    case ioe_errno exc of
                                        Nothing -> liftIO getErrno >>= sendError h
                                        Just e -> sendError h (Errno e)
                                Right block -> sendReplyData h block
                    loop'
                Write h _ _ _ -> sendError h ePERM >> loop'
                Disconnect _ -> return ()
                Flush h _ -> sendError h eOPNOTSUPP >> loop'
                Trim h _ _ _ -> sendError h eOPNOTSUPP >> loop'
                UnknownCommand{} -> do
                    liftIO $ putStrLn $ "Unknown command: " ++ show req
                    loop'

    flags = [HasFlags, ReadOnly]

main :: IO ()
main = runResourceT $ do
    args <- liftIO getArgs

    when (null args) $ liftIO $ do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " exportFile1 (exportFile2 ...)"
        exitFailure

    exports <- forM args $ \fn -> do
        let fn' = Text.pack fn
        (_, fd) <- allocate
                (liftIO $ openFd fn IO.ReadOnly Nothing defaultFileFlags)
                closeFd
        size <- fileSize `fmap` liftIO (getFdStatus fd)
        return (fn', Export fd (fromIntegral size))

    let exportsMap = Map.fromList exports

    void $ resourceForkIO $ runTCPServer settings (application exportsMap)
    forever $ liftIO $ threadDelay 10000000
  where
    settings = serverSettings nBD_DEFAULT_PORT HostIPv4
