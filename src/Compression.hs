module Compression
(
  Compressor(..),
  Method(..)
) where

import Compression.Base
import Compression.RLE
import Compression.Huffman

data Method = RLE | Huffman deriving (Show, Eq)

instance Compressor Method where
  compress RLE = compress CompressionRLE
  compress Huffman = compress CompressionHuff
  extract RLE = extract CompressionRLE
  extract Huffman = extract CompressionHuff
