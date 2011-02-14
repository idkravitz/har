module Huffman
( compress_huffman
, extract_huffman
) where

import ArchiveCommon(Stream, Byte)
import qualified Data.ByteString.Lazy as L
import Data.Array.Diff
import Data.Array.IArray
import Data.Bits(setBit)
import Debug.Trace
import Data.Char(ord)

data Tree = Leaf Int Byte | Branch Int Tree Tree deriving(Show)
data Bit = Zero | One deriving(Eq, Show)
type Bits = [Bit]

getCount (Leaf c _)     = c
getCount (Branch c _ _) = c

countBytes :: Stream -> DiffUArray Byte Int
countBytes s = L.foldl (\ a b -> a // [(b, (a ! b) + 1)]) counts s
    where counts :: DiffUArray Byte Int
          counts = array (0, maxBound::Byte) (zip [0..255] $ cycle [0])

getTree (t:ts) = work t [] ts
    where work x xs []                  = (x, xs)
          work x xs (y:ys)
              | getCount y < getCount x = work y (x:xs) ys
              | otherwise               = work x (y:xs) ys

type CodeBook = DiffArray Byte Bits

createCodebook :: Tree -> DiffArray Byte Bits
createCodebook t = array (0, 255) (work [] t)
    where
      work bs (Leaf _ x) = [(x, bs)]
      work bs (Branch _ t0 t1) = work (bs ++ [Zero]) t0 ++ work (bs ++ [One]) t1

encoded_bits :: CodeBook -> Stream -> Bits
encoded_bits cb s
    | L.null s = []
    | otherwise = (cb ! L.head s) ++ encoded_bits cb (L.tail s)

bitsToStream :: Bits -> Stream
bitsToStream [] = L.empty
bitsToStream bs = toByte h `L.cons` bitsToStream t
    where (h, t) = splitAt 8 bs
          toByte bs = foldl (\ a (b, i) -> if b == One then setBit a i else a)
                      0 (zip bs [7,6..0])

encode :: DiffArray Byte Bits -> Stream -> Stream
encode cb = bitsToStream .  encoded_bits cb

buildTree = build . map (\ (b, c) -> Leaf c b) . filter (\ (b, c) -> c /= 0)
    where
      build [t] = t
      build ts =
          let (t0, ts0) = getTree ts
              (t1, ts1) = getTree ts0
          in build $ Branch (getCount t0 + getCount t1) t0 t1 : ts1


compress_huffman s = let t = buildTree . assocs . countBytes $ s
                         cb = createCodebook t
                     in show t `trace` encode cb s
extract_huffman  = id
