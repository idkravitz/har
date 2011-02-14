module Huffman
( compress_huffman
, extract_huffman
) where

import Control.Monad
import Control.Applicative
import ArchiveCommon(Stream, Byte)
import qualified Data.ByteString.Lazy as L
import Data.Array.Diff
import Data.Array.IArray
import Data.Bits(setBit, testBit)
import Data.Char(ord)
import Data.Int(Int64)
import qualified Data.Binary as Binary

data Tree = Leaf Int Byte | Branch Int Tree Tree deriving(Show)
data Bit = Zero | One deriving(Eq, Show)
type Bits = [Bit]

instance Binary.Binary Tree where
    put (Leaf count byte) = do Binary.put (0 :: Byte)
                               Binary.put count
                               Binary.put byte

    put (Branch count t0 t1) = do Binary.put (1 :: Byte)
                                  Binary.put count
                                  Binary.put t0
                                  Binary.put t1
    get = do t <- Binary.get :: Binary.Get Byte
             case t of
                  0 -> liftM2 Leaf Binary.get Binary.get
                  1 -> liftM3 Branch Binary.get Binary.get Binary.get

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
encode cb = bitsToStream . encoded_bits cb

streamToBits :: Stream -> Bits
streamToBits s
    | L.null s  = []
    | otherwise = (getBit x) ++ streamToBits xs
    where (x, xs) = (L.head s, L.tail s)
          getBit x = foldl (\ a i -> (if testBit x i then One else Zero) : a) [] [0..7]

decode :: Tree -> Stream -> Stream
decode t s = bitDecode t . streamToBits $ s
    where bitDecode :: Tree -> Bits -> Stream
          bitDecode (Branch _ t0 t1) (b:bs)
              | b == Zero = bitDecode t0 bs
              | b == One  = bitDecode t1 bs
          bitDecode (Leaf _ b) bs = b `L.cons` bitDecode t bs

buildTree = build . map (\ (b, c) -> Leaf c b) . filter (\ (b, c) -> c /= 0)
    where
      build [t] = t
      build ts =
          let (t0, ts0) = getTree ts
              (t1, ts1) = getTree ts0
          in build $ Branch (getCount t0 + getCount t1) t0 t1 : ts1

compress_huffman s = let t = buildTree . assocs . countBytes $ s
                         cb = createCodebook t
                     in Binary.encode (t, L.length s, encode cb $ s)

extract_huffman s = let (t, l, bs) = (Binary.decode s) :: (Tree, Int64, Stream)
                    in L.take l . decode t $ bs
