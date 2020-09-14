-- |

module HuffmanSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Compression
import Data.ByteString.Lazy as L
import ArbInstances

compress = caCompress huffmanAlg
extract = caExtract huffmanAlg

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "Basic sanity" $ do
    it "compresses empty to empty" $
      compress (pure L.empty) `shouldReturn` L.empty
    it "extracts empty to empty" $
      extract (pure L.empty) `shouldReturn` L.empty
    it "isomorphic on random ByteString" $
      property $ \(ArbRandByteString s) -> extract (compress (pure s)) `shouldReturn` s
    it "ismorphic on random chunked ByteString" $
      property $ \(ArbByteStringRepChunks s) -> extract (compress (pure s)) `shouldReturn` s
