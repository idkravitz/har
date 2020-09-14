-- |

module ArbInstances(
  ArbByteStringRepChunks(..)
  , ArbRandByteString(..)
) where

import Control.Monad
import Test.QuickCheck
import Data.ByteString.Lazy as L

newtype ArbByteStringRepChunks = ArbByteStringRepChunks { unArbByteStringRepChunks :: ByteString }
  deriving (Show)

newtype ArbRandByteString = ArbRandByteString { unArbRandByteString :: ByteString }
  deriving (Show)

instance Arbitrary ArbByteStringRepChunks where
  arbitrary = do
    chunksCount <- choose (0, 100)
    chunks <- replicateM chunksCount ((,) <$> arbitrary <*> choose (1, 100))
    return $ ArbByteStringRepChunks $ foldMap (\(b, c) -> L.replicate c b) chunks

instance Arbitrary ArbRandByteString where
  arbitrary = do
    len <- choose (0, 1000)
    (ArbRandByteString . L.pack) <$> replicateM len arbitrary
