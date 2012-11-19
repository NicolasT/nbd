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

{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main (main) where

import Prelude hiding (log)

import Control.Applicative

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)

import Control.Monad.State

import Data.Conduit
import Data.Conduit.TMChan

import qualified Data.Map as Map

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text ()

import System.IO (Handle, hPutStrLn, hFlush, stdout)

import Foreign.C.Error (eINVAL)

import Network.NBD hiding (Handle)
import qualified Network.NBD.Client as C
import qualified Network.NBD.Server as S

server :: MonadIO m => Logger -> MVar () -> Conduit ByteString m ByteString
server logger m = do
    log' "Start newstyle negotiation"
    target <- S.negotiateNewstyle (Just ["abc", "def"])
    log' $ "Client selected export " ++ show target ++ ", sending export information"
    S.sendExportInformation (fromIntegral $ LBS.length dat0) [HasFlags]
    log' "Negotiation phase done, start server loop"
    loop dat0
  where
    log' msg = log logger $ "[server] " ++ msg
    dat0 = LBS.replicate 4096 0

    loop dat = do
        log' "Awaiting client request"
        req <- S.getCommand
        log' $ "Received request: " ++ show req
        case req of
            S.Write h o d _ -> do
                let (part0, tail') = LBS.splitAt (fromIntegral o) dat
                    part2 = LBS.drop (LBS.length d) tail'
                    dat' = LBS.concat [part0, d, part2]
                S.sendReply h
                loop dat'
            S.Flush h _ -> S.sendReply h >> loop dat
            S.Trim h _ _ _ -> S.sendReply h >> loop dat
            S.Read h o l _ ->
                if (o > maxBound - fromIntegral l) || (o + fromIntegral l > fromIntegral (LBS.length dat))
                    then S.sendError h eINVAL
                    else do
                        let res = LBS.take (fromIntegral l) $ LBS.drop (fromIntegral o) dat
                        S.sendReplyData h res
                        loop dat
            S.Disconnect _ -> do
                log' "Disconnect"
                liftIO $ putMVar m ()
            S.UnknownCommand {} -> do
                log' "Unknown command"
                loop dat

client :: MonadIO m => Logger -> GLConduit ByteString m ByteString
client logger = do
    log' "Connected, starting negotiation"
    C.negotiateNewstyle

    log' "Requesting export list"
    (Just exports) <- C.optionList
    log' $ "Exports: " ++ show exports

    let export = head exports
    log' $ "Selecting export " ++ show export
    (size, flags) <- C.optionExportName export
    log' $ "Export information: size=" ++ show size ++ ", flags=" ++ show flags

    flip evalStateT (C.handle0, Map.empty, Map.empty) $ do
        let handleResponse = do
                (_, m1, m2) <- get
                resp <- lift $ C.getResponse ((Map.!) m1)
                let handle = case resp of
                                C.Success h -> h
                                C.Error h _ -> h
                                C.Data h _ -> h

                case Map.lookup handle m2 of
                    Nothing -> return ()
                    Just l -> mapM_ (\f -> liftIO $ f resp) l

            wrap size' act exec = do
                handle <- do
                    (h, m1, m2) <- get

                    let m2' = case act of
                                Just act' -> Map.insert h [act'] m2
                                Nothing -> m2

                    put (C.newHandle h, Map.insert h size' m1, m2')

                    return h

                lift $ exec handle

            read' flags' offset len act =
                wrap (Just len) act (\handle -> C.read handle flags' offset len)

            write' flags' offset len dat act =
                wrap Nothing act (\handle -> C.write handle flags' offset len dat)

            flush' flags' act =
                wrap Nothing act (flip C.flush flags')

        write' [ForceUnitAccess] 1024 1024 (LBS.replicate 1024 65) $
            Just (\response -> log' $ "Write result: " ++ show response)
        read' [] 992 64 $
            Just (\response -> log' $ "Read result: " ++ show response)

        handleResponse

        flush' [] $
            Just (\response -> log' $ "Flush result: " ++ show response)

        handleResponse
        handleResponse

    log' "Disconnecting"
    C.disconnect (C.newHandle (C.newHandle C.handle0)) []
    log' "Disconnected"
  where
    log' m = log logger $ "[client] " ++ m

main :: IO ()
main = do
    -- Create two channels we'll use to link the 'client' to the 'server',
    -- and corresponding Sources and Sinks
    clientSinkChan <- newTMChanIO
    clientSourceChan <- newTMChanIO

    let clientSink = sinkTMChan clientSinkChan
        serverSource = sourceTMChan clientSinkChan
        clientSource = sourceTMChan clientSourceChan
        serverSink = sinkTMChan clientSourceChan

    -- Create and launch a logger
    logger <- newLogger
    void $ forkIO $ runLogger logger stdout

    -- Run server 'thread' for client
    sv <- newEmptyMVar
    void $ forkIO $
        serverSource $= server logger sv $$ serverSink

    -- Run the client actions
    clientSource $= client logger $$ clientSink

    -- Wait for client connection handler thread to quit
    -- This will only happen due to the client being disconnected
    takeMVar sv

    -- Stop the logger
    stopLogger logger


-- Some simple logging infrastructure
data LogCommand = Log String
                | Quit
data Logger = Logger (TMChan LogCommand) (MVar ())

newLogger ::(Functor m, Applicative m, MonadIO m) => m Logger
newLogger = Logger <$> liftIO newTMChanIO <*> liftIO newEmptyMVar

runLogger :: MonadIO m => Logger -> Handle -> m ()
runLogger (Logger chan mvar) handle = liftIO loop
  where
    loop = do
        a <- atomically $ readTMChan chan
        case a of
            Nothing -> return ()
            Just (Log msg) -> hPutStrLn handle msg >> loop
            Just Quit -> liftIO $ hFlush handle >> putMVar mvar ()

stopLogger :: MonadIO m => Logger -> m ()
stopLogger (Logger chan mvar) = liftIO $ atomically (writeTMChan chan Quit) >> takeMVar mvar

log :: MonadIO m => Logger -> String -> m ()
log (Logger chan _) = liftIO . atomically . writeTMChan chan . Log
