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

module Network.NBD.Constants (
      nBD_DEFAULT_PORT
    , nBD_INIT_PASSWD
    , nBD_CLISERV_MAGIC
    , nBD_OPTS_MAGIC
    , nBD_REP_MAGIC
    , nBD_REP_FLAG_ERROR
    , nBD_REQUEST_MAGIC
    , nBD_REPLY_MAGIC
    , NbdOption(..)
    , NbdReply(..)
    , NbdExportFlag(..)
    , NbdServerProtocolFlag(..)
    , NbdClientProtocolFlag(..)
    , nBD_CMD_MASK_COMMAND
    , NbdCommandFlag(..)
    ) where

import Data.Bits
import Data.Word

import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as BS8

-- | The default (IANA-assigned) port for NBD
nBD_DEFAULT_PORT :: Int
nBD_DEFAULT_PORT = 10809

nBD_INIT_PASSWD :: BS8.ByteString
nBD_INIT_PASSWD = BS8.pack "NBDMAGIC"
nBD_CLISERV_MAGIC :: Word64
nBD_CLISERV_MAGIC = 0x00420281861253
nBD_OPTS_MAGIC :: Word64
nBD_OPTS_MAGIC = 0x49484156454F5054
nBD_REP_MAGIC :: Word64
nBD_REP_MAGIC = 0x3e889045565a9

nBD_REQUEST_MAGIC :: Word32
nBD_REQUEST_MAGIC = 0x25609513
nBD_REPLY_MAGIC :: Word32
nBD_REPLY_MAGIC = 0x67446698

data NbdOption = ExportName
               | Abort
               | List
               | UnknownOption
  deriving (Show, Eq)

instance Enum NbdOption where
    toEnum n = case n of
        1 -> ExportName
        2 -> Abort
        3 -> List
        _ -> UnknownOption

    fromEnum a = case a of
        ExportName -> 1
        Abort -> 2
        List -> 3


data NbdReply = Ack
              | Server
              | ErrorUnsupported
              | ErrorPolicy
              | ErrorInvalid
              | ErrorPlatform
  deriving (Show, Eq)

nBD_REP_FLAG_ERROR :: Int
nBD_REP_FLAG_ERROR = 1 `shiftL` 31

instance Enum NbdReply where
    toEnum n
      | n == 1 = Ack
      | n == 2 = Server
      | n == 1 .|. nBD_REP_FLAG_ERROR = ErrorUnsupported
      | n == 2 .|. nBD_REP_FLAG_ERROR = ErrorPolicy
      | n == 3 .|. nBD_REP_FLAG_ERROR = ErrorInvalid
      | n == 4 .|. nBD_REP_FLAG_ERROR = ErrorPlatform

    fromEnum a = case a of
        Ack -> 1
        Server -> 2
        ErrorUnsupported -> 1 .|. nBD_REP_FLAG_ERROR
        ErrorPolicy -> 2 .|. nBD_REP_FLAG_ERROR
        ErrorInvalid -> 3 .|. nBD_REP_FLAG_ERROR
        ErrorPlatform -> 4 .|. nBD_REP_FLAG_ERROR


-- | Flags of a single export
data NbdExportFlag = HasFlags    -- ^ Should always be set
                   | ReadOnly    -- ^ The export is read-only
                   | SendFlush   -- ^ The server supports NBD_CMD_FLUSH commands
                   | SendFua     -- ^ The server supports the NBD_CMD_FLAG_FUA flag
                   | Rotational  -- ^ The client should schedule I/O accesses as for a rotational medium
                   | SendTrim    -- ^ The server supports NBD_CMD_TRIM commands
  deriving (Show, Eq)

instance Enum NbdExportFlag where
    toEnum n
      | n == 1 `shiftL` 0 = HasFlags
      | n == 1 `shiftL` 1 = ReadOnly
      | n == 1 `shiftL` 2 = SendFlush
      | n == 1 `shiftL` 3 = SendFua
      | n == 1 `shiftL` 4 = Rotational
      | n == 1 `shiftL` 5 = SendTrim

    fromEnum a = case a of
        HasFlags -> 1 `shiftL` 0
        ReadOnly -> 1 `shiftL` 1
        SendFlush -> 1 `shiftL` 2
        SendFua -> 1 `shiftL` 3
        Rotational -> 1 `shiftL` 4
        SendTrim -> 1 `shiftL` 5


data NbdServerProtocolFlag = ServerFixedNewstyle
  deriving (Show, Eq)

instance Enum NbdServerProtocolFlag where
    toEnum n
      | n == 1 `shiftL` 0 = ServerFixedNewstyle

    fromEnum a = case a of
        ServerFixedNewstyle -> 1 `shiftL` 0


data NbdClientProtocolFlag = ClientFixedNewstyle
  deriving (Show, Eq)

instance Enum NbdClientProtocolFlag where
    toEnum n
      | n == 1 `shiftL` 0 = ClientFixedNewstyle

    fromEnum a = case a of
        ClientFixedNewstyle -> 1 `shiftL` 0

nBD_CMD_MASK_COMMAND :: Word32
nBD_CMD_MASK_COMMAND = 0x0000ffff

-- | Flags set on NBD commands
data NbdCommandFlag = ForceUnitAccess  -- ^ Force Unit Access on NBD_CMD_WRITE
  deriving (Show, Eq)

instance Enum NbdCommandFlag where
    toEnum n
      | n == 1 `shiftL` 16 = ForceUnitAccess

    fromEnum a = case a of
        ForceUnitAccess -> 1 `shiftL` 16
