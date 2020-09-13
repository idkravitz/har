module RLESpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Compression
import Data.ByteString.Lazy

oneByte :: ByteString
oneByte = pack [42]

thousandSameBytes :: ByteString
thousandSameBytes = pack $ Prelude.replicate 1000 42

compress = caCompress rleAlg
extract = caExtract rleAlg

spec :: Spec
spec = do
  describe "RLE - message isomorphism" $ do
    it "compresses empty to empty" $
      compress (pure empty) `shouldReturn` empty
    it "extracts empty to empty" $
      extract (pure empty) `shouldReturn` empty
    it "isomorphic on 1 byte sequence of 42" $
      extract (compress (pure oneByte)) `shouldReturn` oneByte
    it "isomorphic on 1000 byte sequence of 42" $
      extract (compress (pure thousandSameBytes)) `shouldReturn` thousandSameBytes
