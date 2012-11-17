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

{-# LANGUAGE DeriveDataTypeable #-}

module Network.NBD.Types (
      ExportName
    , ExportSize
    , Offset
    , Length
    , Command(..)
    , Handle(Handle)
    , ProtocolException(..)
    ) where

import Data.Word
import Data.Typeable (Typeable)
import Data.Serialize (Serialize(..))

import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS

import Control.Exception.Base

import Network.NBD.Constants

type ExportName = Text
type ExportSize = Word64
type Offset = Word64
type Length = Word32

newtype Handle = Handle Word64
  deriving (Show, Eq)

instance Serialize Handle where
    get = Handle `fmap` get
    {-# INLINE get #-}
    put (Handle h) = put h
    {-# INLINE put #-}


data Command = Read { readHandle :: !Handle
                    , readFrom :: !Offset
                    , readLength :: !Length
                    , readFlags :: [NbdCommandFlag]
                    }
             | Write { writeHandle :: !Handle
                     , writeFrom :: !Offset
                     , writeData :: LBS.ByteString
                     , writeFlags :: [NbdCommandFlag]
                     }
             | Disconnect { disconnectFlags :: [NbdCommandFlag]
                          }
             | Flush { flushHandle :: !Handle
                     , flushFlags :: [NbdCommandFlag]
                     }
             | Trim { trimHandle :: !Handle
                    , trimFrom :: !Offset
                    , trimLength :: !Length
                    , trimFlags :: [NbdCommandFlag]
                    }
             | UnknownCommand { unknownCommandId :: !Word32
                              , unknownCommandHandle :: !Handle
                              , unknownCommandOffset :: !Offset
                              , unknownCommandLength :: !Length
                              , unknownCommandFlags :: [NbdCommandFlag]
                              }
  deriving (Show)

data ProtocolException = InvalidClientFlags !Word32
                       | InvalidMagic String !Word64
                       | ParseFailure String
                       | ClientAbort
  deriving (Show, Typeable)

instance Exception ProtocolException
