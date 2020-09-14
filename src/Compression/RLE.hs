{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Compression.RLE
(
  rleAlg
) where

import Compression.Base
import qualified Data.ByteString.Lazy as L

rleAlg :: CompressionAlgorithm
rleAlg = CompAlg {
  caCompress = compressM,
  caExtract  = extractM
}

pattern Empty :: Stream
pattern Empty <- (L.uncons -> Nothing)
pattern (:>) :: Byte -> Stream -> Stream
pattern x :> xs <- (L.uncons -> Just (x, xs))

compressM, extractM :: (Monad m) => m Stream -> m Stream
compressM = fmap compressRLE
extractM = fmap extractRLE

-- The algorithmic part start here

compactBound :: Num a => a -- might require NoMonomorphismRestriction
compactBound = 2 -- how many equal bytes are used to indicate RLE-compressed part

compressRLE :: Stream -> Stream
compressRLE stream = let
  groups = L.group stream
  f orig@(x :> _) = let
    total = L.length orig
    (n, r) = total `divMod` (fromIntegral (maxBound :: Byte) + compactBound)
    remain | r < compactBound = replicate (fromIntegral r) x
           | otherwise        = [x, x, fromIntegral $ r - compactBound]
    in L.pack $ mconcat (replicate (fromIntegral n) [x, x, maxBound]) ++ remain
  f _ = error "Impossible due to def of L.group"
  in foldMap f groups

extractRLE  :: Stream -> Stream
extractRLE Empty = L.empty
extractRLE (x :> nxt@(y :> (c :> xs)))
 -- count starts from 0 after at least 2 repeatables
  | x == y    = L.replicate (fromIntegral c + compactBound) x <> extractRLE xs
  | otherwise = x `L.cons` extractRLE nxt
extractRLE s = s -- copy as-is
