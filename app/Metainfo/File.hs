{-# LANGUAGE OverloadedStrings #-}
module Metainfo.File where
import Data.Map
import Data.ByteString
import BEncoding.Decode

type InfoDict = Map ByteString Int

data MetainfoFile = MetainfoFile {
    info :: InfoDict, 
    announce :: ByteString,
    announceList :: Maybe [[ByteString]],
    creationDate :: Int,
    comment :: ByteString,
    createdBy :: ByteString,
    encoding :: ByteString
}
instance DecodeBEncoding MetainfoFile where
    decode obj = withObject obj $ 
           MetainfoFile
           <$> getValue "info" 
           <*> getValue "announce"
           <*> getValue "announceList" 
           <*> getValue "creationDate"
           <*> getValue "comment"
           <*> getValue "createdBy"
           <*> getValue "encoding"
