-- * The Haskell Archiver * --

import System.Environment
import System.Exit
import System.IO.Error
import Optparse
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Maybe(fromMaybe)

type Stream = L.ByteString
type Byte = Word8

nextSymbol :: Stream -> Byte
nextSymbol s = (L.head s + 1) `rem` maxB
    where maxB = maxBound::Byte

compress s = rle (nextSymbol s) s $ 0
    where
        maxB = maxBound :: Byte
        rle :: Byte -> Stream -> Byte -> Stream
        rle p s c
            | L.null s  = if c == 0 then s else p `L.cons` L.singleton c
            | c == maxB = p `L.cons` (c `L.cons` rle (nextSymbol s) s 0)
            | p == x    = rle x xs (c + 1)
            | c == 0    = x `L.cons` (rle x xs 0)
            | otherwise = p `L.cons` (c `L.cons` (rle p s 0))
            where x = L.head s
                  xs = L.tail s
        
extract s = rle (nextSymbol s) s
    where rle p s
            | L.null s = L.empty
            | p == x = let counter = L.head xs
                           next    = L.tail xs
                           prev    = L.head xs
                       in
                       foldl (\acc _ -> p `L.cons` acc) (rle (nextSymbol next) next) 
                        $ replicate (fromIntegral . L.head $ xs) 0
            | otherwise = x `L.cons` (rle x xs)
            where x = L.head s
                  xs = L.tail s

main = do
    args <- getArgs
    -- nopts is file list
    (opts, nopts) <- catch (archiverOpts args) 
                           (\err -> do putStrLn $ ioeGetErrorString err
                                       exitWith $ ExitFailure 1)
    content <- L.readFile $ head nopts
    let output = (if optCompress opts then compress else extract) content
    L.writeFile (fromMaybe "" . optOutput $ opts) output
