-- * The Haskell Archiver * --

import System.Environment
import System.Exit
import System.IO.Error
import Optparse
import Data.Maybe(fromMaybe)
import RLE(compressRLE, extractRLE)
import Huffman(compressHuffman, extractHuffman)
import ArchiveCommon(Stream, Byte)
import qualified Data.ByteString.Lazy as L

compress         :: Method -> IO Stream -> IO Stream
extract          :: Method -> IO Stream -> IO Stream
compress RLE     = compressRLE
compress Huffman = compressHuffman
extract  RLE     = extractRLE
extract  Huffman = extractHuffman

produceCompression :: String -> [String] -> Method -> IO ()
produceCompression output inputs method = return()

produceExtraction :: String -> Method -> IO ()
produceExtraction input method = return ()

main = do
    (opts, nopts) <- catch (getArgs >>= archiverOpts) $ \ err -> do
                        putStrLn $ ioeGetErrorString err
                        exitWith $ ExitFailure 1
    
    let outFile = fromMaybe "" . optOutput $ opts
        method   = optMethod opts

-- if func == compress then for each input file we ask for a size on
-- disk, then we write its name and its size (that were on disk), then
-- we write a compressed stream
-- if func == extract then we assert that there is only one input file
-- we ignore the optOutput, and read in a loop name, size, then we
-- extract only size-bytes.

    if optCompress opts then produceCompression outFile nopts method
       else produceExtraction (head nopts) method

--    forM nopts \ inFile -> do
--        func     = if optCompress opts then compress else extract
--        method   = optMethod opts
--    func method (L.readFile in_file) >>= L.writeFile out_file
