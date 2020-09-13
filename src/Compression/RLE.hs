module Compression.RLE
(
CompressionRLE(..)
) where

import ArchiveCommon(Stream, Byte)
import qualified Data.ByteString.Lazy as L
import Compression.Base

data CompressionRLE = CompressionRLE
  deriving (Show)

instance Compressor CompressionRLE where
  compress _ = compressRLE
  extract _ = extractRLE


nextSymbol   :: Stream -> Byte
nextSymbol s = (L.head s + 1) `rem` maxB
    where maxB = maxBound::Byte

compressRLE  :: IO Stream -> IO Stream 
compressRLE s = return . (\s -> rle (nextSymbol s) s  Nothing) =<< s
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

extractRLE  :: IO Stream -> IO Stream 
extractRLE s = return . (\s -> rle (nextSymbol s)  s) =<< s
    where rle p s
            | L.null s  = L.empty
            | p == x    = let counter = L.head xs
                              next    = L.tail xs
                          in
                          foldl (flip L.cons) (rle (nextSymbol next) next)
                              $ replicate (fromIntegral counter + 1) x
            | otherwise = x `L.cons` rle x xs
            where x  = L.head s
                  xs = L.tail s

