{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module BEncoding.Encode where

import BEncoding.Parser
import Data.Map
import Data.ByteString.Char8 as B
import Data.Monoid

class EncodeBEncoding a where
    encode :: a -> BEncodingValue

instance EncodeBEncoding Int where
    encode i = BInt i

instance EncodeBEncoding Bool where
    encode False = BInt 0
    encode True = BInt 1

instance EncodeBEncoding ByteString where
    encode str = BString str

instance (EncodeBEncoding a) => EncodeBEncoding [a] where
    encode = BList . fmap encode

instance (EncodeBEncoding a) => EncodeBEncoding (Map ByteString a) where
    encode = BDict . fmap encode

serialize :: BEncodingValue -> ByteString
serialize (BString str) = (pack $ show (B.length str)) <> ":" <> str
serialize (BInt i)      = "i" <> (pack $ show i) <> "e"
serialize (BList xs)    = "l" <> foldMap serialize xs <> "e"
serialize (BDict dict)  = "d" <> (foldMap (\(k,v) -> k <> serialize v) $ toList dict) <> "e"
  


setDict :: [(ByteString, BEncodingValue)] -> BEncodingValue
setDict = BDict . fromList 
