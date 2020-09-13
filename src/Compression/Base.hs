module Compression.Base
( CompressionAlgorithm(..)
, Stream
, Byte
) where

import Data.Word(Word8)
import Data.ByteString.Lazy(ByteString)

type Stream = ByteString
type Byte   = Word8

data CompressionAlgorithm = CompAlg {
  caCompress :: IO Stream -> IO Stream,
  caExtract  :: IO Stream -> IO Stream
}
