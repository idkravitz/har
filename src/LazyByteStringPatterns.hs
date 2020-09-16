{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module LazyByteStringPatterns(
  pattern Empty,
  pattern (:>)
                             ) where

import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)

pattern Empty :: L.ByteString
pattern Empty <- (L.uncons -> Nothing)
pattern (:>) :: Word8 -> L.ByteString -> L.ByteString
pattern x :> xs <- (L.uncons -> Just (x, xs))
