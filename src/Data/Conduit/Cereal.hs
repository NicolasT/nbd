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

import Data.Conduit hiding (leftover)
import Data.Conduit.Util hiding (sinkState)
import Data.Conduit.List (sourceList)
import Data.Conduit.Internal (Pipe(Done, Leftover, NeedInput, PipeM))

import Data.Serialize (Get, Put)
import qualified Data.Serialize as S

import Control.Monad.Trans (lift)

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
sinkGet get = sinkState state0 push close
  where
    state0 = S.runGetPartial get
    {-# INLINE state0 #-}
    push state input = case state input of
        S.Fail message -> return $ StateDone (Just input) (Left message)
        S.Partial state' -> return $ StateProcessing state'
        S.Done result leftover -> return $ StateDone (Just leftover) (Right result)
    {-# INLINE push #-}
    close _ = return $ Left "too few bytes\nFrom:\tsinkGet\n\n"
    {-# INLINE close #-}


-- | Generalized version of 'Data.Conduit.Util.sinkState'
sinkState
    :: Monad m
    => state -- ^ initial state
    -> (state -> input -> m (SinkStateResult state input output)) -- ^ push
    -> (state -> m output) -- ^ Close. Note that the state is not returned, as it is not needed.
    -> GLSink input m output
sinkState state0 push0 close0 =
    NeedInput (push state0) (\_ -> close state0)
  where
    push state input = PipeM
        (do
            res <- state `seq` push0 state input
            case res of
                StateProcessing state' -> return $ NeedInput (push state') (\_ -> close state')
                StateDone mleftover output -> return $ maybe id (flip Leftover) mleftover $ Done output)

    close = lift . close0
