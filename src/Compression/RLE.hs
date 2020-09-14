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

compressRLE :: Stream -> Stream
compressRLE stream = let
  groups = L.group stream
  f orig@(x :> _) = let
    total = L.length orig
    (n, r) = total `divMod` 257 -- maxBound :: Byte + 2
    remain | r < 2     = replicate (fromIntegral r) x
           | otherwise = [x, x, fromIntegral $ r - 2]
    in L.pack $ mconcat (remain : replicate (fromIntegral n) [x, x, maxBound])
  f _ = error "Impossible due to def of L.group"
  in foldMap f groups

extractRLE  :: Stream -> Stream
extractRLE Empty = L.empty
extractRLE (x :> nxt@(y :> (c :> xs)))
  | x == y    = L.replicate (fromIntegral c + 2) x <> extractRLE xs -- count starts from 0 after at least 2 repeatables
  | otherwise = x `L.cons` extractRLE nxt
extractRLE s = s -- copy as-is
