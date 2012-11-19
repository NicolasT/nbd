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

{-# LANGUAGE Rank2Types #-}

module Network.NBD.Utils (
      sinkGet'
    ) where

import Control.Exception.Base (throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Conduit (GLSink)
import Data.Conduit.Cereal (sinkGet)

import Data.Serialize (Get)
import Data.ByteString (ByteString)

import Network.NBD.Types (ProtocolException(ParseFailure))

sinkGet' :: MonadIO m => Get a -> GLSink ByteString m a
sinkGet' g = do
    r <- sinkGet g
    case r of
        Right v -> return v
        Left s -> liftIO $ throwIO $ ParseFailure s
{-# INLINE sinkGet' #-}
