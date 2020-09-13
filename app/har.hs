import Control.Exception
import System.Environment
import System.Exit
import System.IO.Error
import Optparse
import Data.Maybe(fromMaybe)
import Compression
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    (opts, nopts) <- catch (getArgs >>= archiverOpts) $ \ err -> do
                        putStrLn $ ioeGetErrorString err
                        exitWith $ ExitFailure 1
    let in_file  = head nopts
        out_file = fromMaybe "" . optOutput $ opts 
        func     = if optCompress opts then compress else extract
        method   = optMethod opts
    func method (L.readFile in_file) >>= L.writeFile out_file
