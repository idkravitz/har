module ArchiveCommon
( Stream
, Byte
) where

import Data.Word(Word8)
import Data.ByteString.Lazy(ByteString)

type Stream = ByteString
type Byte   = Word8
