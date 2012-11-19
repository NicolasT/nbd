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

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Conduit
import Data.Conduit.Network

import qualified Data.Text as T

import System.IO (hFlush, stdout)

import Network.NBD
import Network.NBD.Client

application :: MonadIO m => Application m
application dat = appSource dat $= handler $$ appSink dat
  where
    handler = do
        negotiateNewstyle
        exports <- optionList

        liftIO $ case exports of
            Nothing -> putStrLn "Export listing not enabled"
            Just e -> do
                putStrLn "Exports:"
                forM_ e $ \export ->
                    putStrLn $ "- " ++ T.unpack export

        name <- liftIO $ do
            putStrLn ""
            putStr "Name of export to query? "
            hFlush stdout
            getLine

        (size, flags) <- optionExportName (T.pack name)
        liftIO $ putStrLn $ name ++ ": size=" ++ show size ++ ", flags=" ++ show flags

        disconnect handle0 []

main :: IO ()
main = runTCPClient settings application
  where
    settings = clientSettings nBD_DEFAULT_PORT "127.0.0.1"
