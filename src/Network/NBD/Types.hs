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
    , Response(..)
    , Handle(Handle)
    , handle0
    , newHandle
    , ProtocolException(..)
    ) where

import Data.Word
import Data.Typeable (Typeable)
import Data.Serialize (Serialize(..))

import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Exception.Base

import Foreign.C.Error (Errno(Errno))

import Network.NBD.Constants

-- | A type synonym for export names
-- In line with the protocol spec, this will be UTF-8 encoded
type ExportName = Text
-- | A type synonym for the size of a single export
type ExportSize = Word64
-- | A type synonym for offset values passed in commands
type Offset = Word64
-- | A type synonym for length values passed in commands
type Length = Word32

-- | Opaque type representing a command handle sent by a client
newtype Handle = Handle Word64
  deriving (Show, Eq, Ord)

instance Serialize Handle where
    get = Handle `fmap` get
    {-# INLINE get #-}
    put (Handle h) = put h
    {-# INLINE put #-}

-- | An initial 'Handle'
handle0 :: Handle
handle0 = Handle 0

-- | Calculate a new 'Handle' given a used one
newHandle :: Handle -> Handle
newHandle (Handle h) = Handle (h + 1)


-- | Representation of a client command
data Command = Read { readHandle :: !Handle          -- ^ Request handle
                    , readFrom :: !Offset            -- ^ Requested offset
                    , readLength :: !Length          -- ^ Requested length
                    , readFlags :: [NbdCommandFlag]  -- ^ Command flags
                    }
             | Write { writeHandle :: !Handle          -- ^ Request handle
                     , writeFrom :: !Offset            -- ^ Write offset
                     , writeData :: LBS.ByteString     -- ^ Data
                     , writeFlags :: [NbdCommandFlag]  -- ^ Command flags
                     }
             | Disconnect { disconnectFlags :: [NbdCommandFlag]  -- ^ Command flags
                          }
             | Flush { flushHandle :: !Handle          -- Request handle
                     , flushFlags :: [NbdCommandFlag]  -- ^ Command flags
                     }
             | Trim { trimHandle :: !Handle          -- ^ Request handle
                    , trimFrom :: !Offset            -- ^ Requested offset
                    , trimLength :: !Length          -- ^ Requested length
                    , trimFlags :: [NbdCommandFlag]  -- ^ Command flags
                    }
             | UnknownCommand { unknownCommandId :: !Word32              -- ^ Request command number
                              , unknownCommandHandle :: !Handle          -- ^ Request handle
                              , unknownCommandOffset :: !Offset          -- ^ Request offset
                              , unknownCommandLength :: !Length          -- ^ Request length
                              , unknownCommandFlags :: [NbdCommandFlag]  -- ^ Command flags
                              }
  deriving (Show)


-- | Server response to a client command
data Response = Success Handle  -- ^ Command succeeded
              | Error Handle Errno  -- ^ Command execution failed with given error
              | Data Handle LBS.ByteString  -- ^ Command succeeded and returned some data

instance Show Response where
    show (Success h) = "Success (" ++ show h ++ ")"
    show (Error h (Errno e)) = "Error (" ++ show h ++ ") (Errno " ++ show e ++ ")"
    show (Data h b) = "Data (" ++ show h ++ ") (" ++ show b ++ ")"


-- | The 'ProtocolException' type lists a couple of values which can be
-- raised at runtime
data ProtocolException = InvalidClientFlags !Word32   -- ^ Client sent invalid flags during negotiation
                       | InvalidMagic String !Word64  -- ^ An invalid magic value was received 
                       | ParseFailure String          -- ^ Parsing of some field failed
                       | ClientAbort                  -- ^ The client sent an NBD_OPT_ABORT option
                       | InvalidProtocolHeader BS.ByteString -- ^ Server sent some invalid/unknown protocol header
                       | InvalidServerFlags !Word16   -- ^ Server sent invalid flags during negotiation
                       -- TODO Not very happy with this one, shouldn't pass
                       -- a Word32 as-is but decode to NbdReply
                       | OptionError !Word32          -- ^ Server returned an error reply to an option command
                       | UnexpectedServerReply String
  deriving (Show, Typeable)

instance Exception ProtocolException
