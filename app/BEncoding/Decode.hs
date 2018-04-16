{-# LANGUAGE FlexibleInstances #-}

module BEncoding.Decode where
import Data.Map
import Data.ByteString
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import BEncoding.Parser

class DecodeBEncoding a where
    decode :: BEncodingValue -> Maybe a

instance DecodeBEncoding Int where
    decode (BInt i) = Just i
    decode _ = Nothing

instance DecodeBEncoding Bool where
    decode (BInt 0) = Just False
    decode (BInt 1) = Just True
    decode _ = Nothing

instance DecodeBEncoding ByteString where
    decode (BString str) = Just str
    decode _ = Nothing

instance (DecodeBEncoding a) => DecodeBEncoding [a] where
    decode (BList xs) = traverse decode xs
    decode _ = Nothing

instance (DecodeBEncoding a) => DecodeBEncoding (Maybe a) where
    decode x = return $ decode x

instance (DecodeBEncoding a) => DecodeBEncoding (Map ByteString a) where
    decode (BDict dict) = traverse decode dict
    decode _ = Nothing

getValueInner :: DecodeBEncoding a => ByteString -> BEncodingValue -> Maybe a
getValueInner key (BDict dict) = Data.Map.lookup key dict >>= decode
getValueInner _ _ = Nothing

getValue :: DecodeBEncoding a => ByteString -> ReaderT BEncodingValue Maybe a
getValue key = ReaderT (getValueInner key)

withObject :: DecodeBEncoding a => BEncodingValue -> ReaderT BEncodingValue Maybe a -> Maybe a
withObject obj reader = runReaderT reader obj
