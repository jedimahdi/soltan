{-# LANGUAGE NoImplicitPrelude #-}

module Hokm.Data.ByteString
    ( trim
    ) where

import           Data.ByteString
import           Data.Word8      ( isSpace )
import           Prelude         ( fst, (.) )

trim :: ByteString -> ByteString
trim = fst . spanEnd isSpace . dropWhile isSpace
{-# INLINE trim #-}
