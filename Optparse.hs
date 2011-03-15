module Optparse
( archiverOpts
, Options(..)
, Method(..))
where

import System.Environment
import System.Console.GetOpt
import System.FilePath((<.>))
import Data.Maybe(fromMaybe)

data Method = RLE | Huffman deriving(Show, Eq)

data Options = Options
    { optHelp     :: Bool
    , optCompress :: Bool
    , optExtract  :: Bool
    , optOutput   :: Maybe FilePath
    , optMethod   :: Method
    } deriving(Show, Eq)

defaultOptions = Options
    { optHelp      = False
    , optCompress  = False
    , optExtract   = False
    , optMethod    = RLE
    , optOutput    = Nothing
    }

options =
    [ Option ['h'] ["help"]
        (NoArg  (\ o -> o { optHelp = True })) "print help message"
    , Option ['c'] ["compress"]
        (NoArg  (\ o -> o { optCompress = True })) "use compression mode"
    , Option ['x'] ["extract"]
        (NoArg  (\ o -> o { optExtract = True })) "use extraction mode"
    , Option ['o'] ["output"]
        (OptArg (\ f o -> o { optOutput = f }) "FILE") "set output file"
    , Option ['r'] ["RLE"]
        (NoArg  (\ o -> o { optMethod = RLE })) "use RLE algorithm (default)"
    , Option ['H'] ["Huffman"]
        (NoArg  (\ o -> o { optMethod = Huffman })) "use Huffman algorithm" 
    ]

helpMsg = usageInfo header options
    where header = "Usage: har [OPTION...] file"

fixOutput (opts, nopts) = (opts { optOutput = Just . outputFileName $ optOutput opts }, nopts)
    where outputFileName Nothing = head nopts <.> ".har"
          outputFileName (Just f) = f

archiverOpts :: [String] -> IO (Options, [String])
archiverOpts argv =
    case getOpt Permute options argv of
        (o, n, []  ) ->
            do
                let opts = (foldl (flip id) defaultOptions o, n)
                    checkOptions o@(opts, nopts)
                        | optHelp opts || null nopts = fail helpMsg
                        | optCompress opts == optExtract opts =
                            fail $ "Compress or extract must be set alone\n" ++ helpMsg
                        | otherwise = return (fixOutput o)
                checkOptions opts
        (_, _, errs) -> fail $ concat errs ++ helpMsg

