{-# LANGUAGE BangPatterns #-}
module Compression.RLE
(
  rleAlg
) where

import Compression.Base
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import LazyByteStringPatterns

rleAlg :: CompressionAlgorithm
rleAlg = CompAlg {
  caCompress = compressM,
  caExtract  = extractM
}

compressM, extractM :: (Monad m) => m Stream -> m Stream
compressM = fmap compressRLE
extractM = fmap extractRLE

-- The algorithmic part start here

compactBound :: Num a => a -- might require NoMonomorphismRestriction
compactBound = 2 -- how many equal bytes are used to indicate RLE-compressed part

compressRLE :: Stream -> Stream
compressRLE stream = let
  groups = L.group stream
  f ~orig@(x :> _) = let
    total = L.length orig
    (!n, !r) = total `divMod` (fromIntegral (maxBound :: Byte) + compactBound)
    remain | r < compactBound = replicate (fromIntegral r) x
           | otherwise        = [x, x, fromIntegral $ r - compactBound]
    in mconcat (replicate (fromIntegral n) (lazyByteString (L.pack [x, x, maxBound])))
       <> lazyByteString (L.pack remain)
  in toLazyByteString $ foldMap f groups

extractRLE :: Stream -> Stream
extractRLE = toLazyByteString . helper where
  helper  :: Stream -> Builder
  helper Empty = mempty
  helper (x :> nxt@(y :> (c :> xs)))
  -- count starts from 0 after at least 2 repeatables
    | x == y    = lazyByteString (L.replicate (fromIntegral c + compactBound) x) <> helper xs
    | otherwise = word8 x <> helper nxt
  helper s = lazyByteString s -- copy as-is
