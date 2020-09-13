module Compression.Base(Compressor(..)) where

import ArchiveCommon(Stream)

class Compressor algo where
  compress, extract :: algo -> IO Stream -> IO Stream
