module RLESpec where

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Compression
import Data.ByteString.Lazy as L
import ArbInstances

oneByte :: ByteString
oneByte = L.pack [42]

thousandSameBytes :: ByteString
thousandSameBytes = L.replicate 1000 42

compress = caCompress rleAlg
extract = caExtract rleAlg


spec :: Spec
spec = do
  modifyMaxSuccess (const 1000) $ describe "Message isomorphism" $ do
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
    it "shouldn't break on byte boundary" $ do
      let s = L.replicate 258 0
      extract (compress (pure s)) `shouldReturn` s
