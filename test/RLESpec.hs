module RLESpec where

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Compression
import Data.ByteString.Lazy as L

oneByte :: ByteString
oneByte = L.pack [42]

thousandSameBytes :: ByteString
thousandSameBytes = L.replicate 1000 42

compress = caCompress rleAlg
extract = caExtract rleAlg

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

spec :: Spec
spec = do
  describe "Message isomorphism" $ do
    it "compresses empty to empty" $
      compress (pure L.empty) `shouldReturn` L.empty
    it "extracts empty to empty" $
      extract (pure L.empty) `shouldReturn` L.empty
    it "isomorphic on 1 byte sequence of 42" $
      extract (compress (pure oneByte)) `shouldReturn` oneByte
    it "isomorphic on 1000 byte sequence of 42" $
      extract (compress (pure thousandSameBytes)) `shouldReturn` thousandSameBytes
    it "has isomorphic property on random ByteString with repeatable chunks" $
      property $ \(ArbByteStringRepChunks s) -> extract (compress (pure s)) `shouldReturn` s
    it "has isomorphic property on randomly random ByteString" $
      property $ \(ArbRandByteString s) -> extract (compress (pure s)) `shouldReturn` s
