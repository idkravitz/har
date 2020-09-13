module Optparse
( archiverOpts
, Options(..)
)
where

import Compression
import System.Console.GetOpt
import System.FilePath((<.>))

data Options = Options
    { optHelp     :: Bool
    , optCompress :: Bool
    , optExtract  :: Bool
    , optOutput   :: Maybe FilePath
    , optAlgorithm :: CompressionAlgorithm
    }

defaultOptions :: Options
defaultOptions = Options
    { optHelp      = False
    , optCompress  = False
    , optExtract   = False
    , optAlgorithm = rleAlg
    , optOutput    = Nothing
    }

options :: [OptDescr (Options -> Options)]
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
        (NoArg  (\ o -> o { optAlgorithm = rleAlg })) "use RLE algorithm (default)"
    , Option ['H'] ["Huffman"]
        (NoArg  (\ o -> o { optAlgorithm = huffmanAlg })) "use Huffman algorithm"
    ]

helpMsg :: String
helpMsg = usageInfo header options
    where header = "Usage: har [OPTION...] file"

fixOutput :: (Options, [FilePath]) -> (Options, [FilePath])
fixOutput (opts, nopts) = (opts { optOutput = Just . outputFileName $ optOutput opts }, nopts)
    where outputFileName Nothing = head nopts <.> ".har"
          outputFileName (Just f) = f

-- TODO: This func is unreadable mess, refactoring needed
-- TODO: It should be any instance of Except, IO is redundant here
archiverOpts :: [String] -> IO (Options, [String])
archiverOpts argv =
    case getOpt Permute options argv of
        (o, n, []  ) ->
            do
                let opts = (foldl (flip id) defaultOptions o, n)
                    checkOptions o'@(opts', nopts)
                        | optHelp opts' || null nopts = fail helpMsg
                        | optCompress opts' == optExtract opts' =
                            fail $ "Compress or extract must be set alone\n" ++ helpMsg
                        | otherwise = return (fixOutput o')
                checkOptions opts
        (_, _, errs) -> fail $ concat errs ++ helpMsg

