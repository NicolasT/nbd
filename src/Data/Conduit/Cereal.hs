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

module Data.Conduit.Cereal (
      sourcePut
    , sourcePutLazy
    , sinkGet
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Data.Conduit
import Data.Conduit.List (sourceList)

import Data.Serialize (Get, Put)
import qualified Data.Serialize as S

-- | Turn a 'Put' into a 'Source', constructing a strict bytestring
sourcePut :: Monad m => Put -> GSource m ByteString
sourcePut = yield . S.runPut
{-# INLINE sourcePut #-}

-- | Turn a 'Put' into a 'Source', yielding chunks of a lazy bytestring
sourcePutLazy :: Monad m => Put -> GSource m ByteString
sourcePutLazy = sourceList . LBS.toChunks . S.runPutLazy
{-# INLINE sourcePutLazy #-}

-- | Turn a 'Get' into a 'Sink'
sinkGet :: Monad m => Get a -> GLSink ByteString m (Either String a)
sinkGet = loop . S.runGetPartial
  where
    loop state = await >>= maybe closed handle
      where
        handle bs = case state bs of
            S.Fail message -> return $ Left message
            S.Partial state' -> loop state'
            S.Done result leftovers -> do
                leftover leftovers
                return $ Right result
    {-# INLINE loop #-}

    closed = return $ Left "too few bytes\nFrom:\tsinkGet\n\n"
    {-# INLINE closed #-}
{-# INLINE sinkGet #-}
