-- {-# LANGUAGE ViewPatterns #-}
-- * The Haskell Archiver * --

import System.Environment
import System.Exit
import System.IO.Error
import Optparse
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Maybe(fromMaybe)

type Stream = L.ByteString
type Byte   = Word8

nextSymbol   :: Stream -> Byte
nextSymbol s = (L.head s + 1) `rem` maxB
    where maxB = maxBound::Byte

compress s = rle (nextSymbol s) s $ Nothing
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

extract s = rle (nextSymbol s) s
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

main = do
    (opts, nopts) <- catch (getArgs >>= archiverOpts)
                           (\ err -> do putStrLn $ ioeGetErrorString err
                                        exitWith $ ExitFailure 1)
    content <- L.readFile $ head nopts
    let output = (if optCompress opts then compress else extract) content
    L.writeFile (fromMaybe "" . optOutput $ opts) output
