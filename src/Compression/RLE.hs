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

-- TODO: What the fuck is this shit?!
nextSymbol   :: Stream -> Byte
nextSymbol s = (L.head s + 1) `rem` maxB
    where maxB = maxBound::Byte

compressM, extractM :: (Monad m) => m Stream -> m Stream
compressM = fmap compressRLE
extractM = fmap extractRLE

compressRLE  :: Stream -> Stream
compressRLE = flip (nextSymbol >>= rle) Nothing
    where
        maxB = maxBound :: Byte
        rle             :: Byte -> Stream -> Maybe Byte -> Stream
        rle p s Nothing
            | L.null s  = L.empty
            | p == x    = rle x xs (Just 0)
            | otherwise = x `L.cons` rle x xs Nothing
            where x  = L.head s
                  xs = L.tail s
        rle p s (Just c)
            | L.null s  = p `L.cons` L.singleton c
            | c == maxB = p `L.cons` (c `L.cons` rle (nextSymbol s) s Nothing)
            | p == x    = rle x xs (Just $ c + 1)
            | otherwise = p `L.cons` (c `L.cons` rle p s Nothing)
            where x  = L.head s
                  xs = L.tail s

extractRLE  :: Stream -> Stream
extractRLE Empty = L.empty
extractRLE (x :> nxt@(y :> (c :> xs)))
  | x == y    = L.replicate (fromIntegral c + 2) x <> extractRLE xs -- count starts from 0 after at least 2 repeatables
  | otherwise = x `L.cons` extractRLE nxt
extractRLE s = s -- copy as-is
