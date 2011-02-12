-- * The Haskell Archiver * --

import System.Environment
import System.Exit
import System.IO.Error
import Optparse
import Data.Maybe(fromMaybe)
import RLE(compress_rle, extract_rle)
import Huffman(compress_huffman, extract_huffman)
import ArchiveCommon(Stream, Byte)
import qualified Data.ByteString.Lazy as L

compress         :: Method -> Stream -> Stream
extract          :: Method -> Stream -> Stream
compress RLE     = compress_rle
compress Huffman = compress_huffman
extract  RLE     = extract_rle
extract  Huffman = extract_huffman

main = do
    (opts, nopts) <- catch (getArgs >>= archiverOpts)
                           (\ err -> do putStrLn $ ioeGetErrorString err
                                        exitWith $ ExitFailure 1)
    content <- L.readFile $ head nopts
    let output = (if optCompress opts then compress
                                      else extract) (optMethod opts) content
    L.writeFile (fromMaybe "" . optOutput $ opts) output
