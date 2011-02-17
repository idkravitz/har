module Huffman
( compress_huffman
, extract_huffman
) where

import Control.Monad
import Control.Monad.ST
import Control.Applicative
import Control.Exception.Base(evaluate)
import ArchiveCommon(Stream, Byte)
import qualified Data.ByteString.Lazy as L
import Data.Bits(setBit, testBit)
import Data.Char(ord)
import Data.Int(Int64)
import qualified Data.Binary as Binary
import Data.Binary.Get
import System.IO.Unsafe(unsafePerformIO)
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.List

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

-- Mutable array used for optimization purpose
-- making a time complicity O(n) with small const
-- DiffArray is too slow for whis task

{-# NOINLINE countBytes #-}
countBytes   :: Stream -> IO [(Byte, Int)]
countBytes s = do
    arr <- counts
    forM_ (L.unpack s) (\ b -> (+ 1) <$> readArray arr b >>= writeArray arr b)
    getAssocs arr
    where counts = newArray (0, 255) 0 :: IO (IOUArray Byte Int)

-- obtains a pair of smallest node and the rest, that
-- are greater
getTree        :: [Tree] -> (Tree, [Tree])
getTree (t:ts) = work t [] ts
    where work x xs []                  = (x, xs)
          work x xs (y:ys)
              | getCount y < getCount x = work y (x:xs) ys
              | otherwise               = work x (y:xs) ys

type CodeBook = Array Byte Bits

createCodebook   :: Tree -> CodeBook
createCodebook t = array (0, 255) (work [] t)
    where
      work bs (Leaf _ x)       = [(x, bs)]
      work bs (Branch _ t0 t1) = work (bs ++ [Zero]) t0 ++ work (bs ++ [One]) t1

encoded_bits    :: CodeBook -> Stream -> Bits
encoded_bits cb s = concatMap (\b -> cb ! b) (L.unpack s)

-- convert bit stream to byte stream
bitsToStream :: Bits -> Stream
bitsToStream [] = L.empty
bitsToStream bs = work 0 bs 7
    where work          :: Byte -> Bits ->Int -> Stream
          work a [] _   = L.singleton a
          work a (b:bs) i
            | i == 0    = (if b == One then setBit a i else a) `L.cons` (work 0 bs 7)
            | b == Zero = work a bs (i - 1)
            | otherwise = work (setBit a i) bs (i - 1)

-- compression with specified codebook
encode :: CodeBook -> Stream -> Stream
encode cb = bitsToStream . encoded_bits cb

-- convert byte stream to bit stream
streamToBits :: Stream -> Bits
streamToBits s
    | L.null s  = []
    | otherwise = (getBit x) ++ streamToBits xs
    where (x, xs) = (L.head s, L.tail s)
          getBit x = foldl' (\ a i -> (if testBit x i then One else Zero) : a) [] [0..7]

-- extraction with specified tree
decode :: Tree -> Stream -> Stream
decode t s = bitDecode t . streamToBits $ s
    where bitDecode :: Tree -> Bits -> Stream
          bitDecode (Branch _ t0 t1) (b:bs)
              | b == Zero = bitDecode t0 bs
              | b == One  = bitDecode t1 bs
          bitDecode (Leaf _ b) bs = b `L.cons` bitDecode t bs

-- create tree from statistics list (pairs of (byte, count))
buildTree = build . map (\ (b, c) -> Leaf c b) . filter (\ (b, c) -> c /= 0)
    where
      build [t] = t -- its leaf
      build ts =
          let (t0, ts0) = getTree ts  -- first smallest and rest that greater
              (t1, ts1) = getTree ts0 -- second smallest and rest that greater
          in build $ Branch (getCount t0 + getCount t1) t0 t1 : ts1

compress_huffman :: IO Stream -> IO Stream
compress_huffman makeInput = do
    len <- (return $!). L.length =<< makeInput
    t   <- (return $!). buildTree =<< countBytes  =<< makeInput
    code <- return . encode (createCodebook t) =<< makeInput 
    return $ Binary.encode (t, len) `L.append` code

deserialize :: Get (Tree, Int64, Stream)
deserialize = liftM3 (,,) Binary.get Binary.get getRemainingLazyByteString

extract_huffman :: IO Stream -> IO Stream
extract_huffman s = do
    (t, l, bs) <- return . runGet deserialize =<< s
    return . L.take l . decode t $ bs
