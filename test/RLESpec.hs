module RLESpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Compression
import Data.ByteString.Lazy

oneByte :: ByteString
oneByte = pack [42]

thousandSameBytes :: ByteString
thousandSameBytes = pack $ Prelude.replicate 1000 42

spec :: Spec
spec = do
  describe "RLE isomorphic" $ do
    it "compresses empty to empty" $
      compress RLE (pure empty) `shouldReturn` empty
    it "extracts empty to empty" $
      extract RLE (pure empty) `shouldReturn` empty
    it "isomorphic on 1 byte sequence of 42" $
      extract RLE (compress RLE (pure oneByte)) `shouldReturn` oneByte
    it "isomorphic on 1000 byte sequence of 42" $
      extract RLE (compress RLE (pure thousandSameBytes)) `shouldReturn` thousandSameBytes
