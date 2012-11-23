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

{-# LANGUAGE PackageImports #-}
module Main (
      main
    ) where

import Data.Bits

import Data.Conduit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text

import Control.Concurrent (threadDelay)

import Control.Monad (forever, forM, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)

import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeSetErrorString, mkIOError, eofErrorType)

import System.Posix.Files (fileSize, getFdStatus)
import System.Posix.IO (closeFd, defaultFileFlags, openFd)
import qualified System.Posix.IO as IO
import "unix-bytestring" System.Posix.IO.ByteString (fdPread)
import System.Posix.Types (Fd)

import System.Posix.IO.Extra (fALLOC_FL_PUNCH_HOLE, fALLOC_FL_KEEP_SIZE, fallocate, fdatasync, fsync, pwriteAllLazy)

import Network.NBD.Server.Simple

data Export = Export { exportHandle :: Fd
                     , exportSize :: ByteCount
                     }
  deriving (Show)

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
                (liftIO $ openFd fn IO.ReadWrite Nothing defaultFileFlags)
                closeFd
        size <- fileSize `fmap` liftIO (getFdStatus fd)
        return (fn', Export fd (fromIntegral size))

    let exportsMap = Map.fromList exports
        exportNames = Just $ Map.keys exportsMap

    void $ resourceForkIO $ liftIO $ runServer settings exportNames (exportMaker exportsMap)
    forever $ liftIO $ threadDelay 10000000
  where
    settings = UNIXServerSettings "nbdsock"
    exportMaker exportsMap _ exportName = case Map.lookup exportName exportsMap of
        Nothing -> return Nothing
        Just e -> return $ Just ExportHandler { handleRead = handleRead' e
                                              , handleWrite = Just $ handleWrite' e
                                              , handleFlush = Just $ handleFlush' e 
                                              , handleTrim = Just $ handleTrim' e 
                                              , handleDisconnect = Nothing
                                              , handleUnknownCommand = Nothing
                                              , handleSize = exportSize e
                                              , handleHasFUA = True
                                              , handleIsRotational = False
                                              }

    handleRead' e o l _ = withBoundsCheck (exportSize e) o l $
        (OK . Data) `fmap` pread (exportHandle e) (fromIntegral l) (fromIntegral o)

    handleWrite' e o d f = withBoundsCheck (exportSize e) o (fromIntegral $ LBS.length d) $ do
        pwriteAllLazy (exportHandle e) d (fromIntegral o)
        when (ForceUnitAccess `elem` f) $
            fdatasync (exportHandle e)
        return $ OK ()

    handleFlush' e _ = fsync (exportHandle e) >> return (OK ())

    handleTrim' e o l f = withBoundsCheck (exportSize e) o l $ do
        fallocate (exportHandle e)
            (fALLOC_FL_PUNCH_HOLE .|. fALLOC_FL_KEEP_SIZE)
            (fromIntegral o)
            (fromIntegral l)
        when (ForceUnitAccess `elem` f) $
            fdatasync (exportHandle e)
        return $ OK ()

    pread :: Fd -> ByteCount -> FileOffset -> IO LBS.ByteString
    pread fd = loop []
      where
        loop acc l o
            | l == 0 = return $ LBS.fromChunks $ reverse acc
            | otherwise = do
                bs <- fdPread fd l o
                let len = BS.length bs
                when (len == 0) $
                    ioError $ ioeSetErrorString (mkIOError eofErrorType "pread" Nothing Nothing) "EOF"
                loop (bs : acc) (l - fromIntegral len) (o + fromIntegral len)
