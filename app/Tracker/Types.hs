{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Tracker.Types where
import Data.ByteString
import BEncoding.Decode
import BEncoding.Encode
import BEncoding.Parser
import Control.Lens

type PeerId = ByteString
type TrackerId = ByteString
type Ip =  [Int]

data TrackerEvent = Started | Stopped | Completed

instance DecodeBEncoding TrackerEvent where
    decode (BString str)
        | str == "Started"   = Just Started
        | str == "Stopped"   = Just Stopped
        | str == "Completed" = Just Completed
    decode _ = Nothing

data TrackerRequest = TrackerRequest { 
    _infoHash :: ByteString, 
    _peerId :: PeerId, 
    _ip :: Maybe Ip, 
    _uploaded :: Int,
    _downloaded :: Int,
    _left :: Int, 
    _compact :: Bool,
    _noPeerId :: Bool,
    _event :: Maybe TrackerEvent,
    _numwant :: Maybe Int,
    _key :: Maybe ByteString,
    _trackerId :: Maybe TrackerId }

makeLenses ''TrackerRequest

instance DecodeBEncoding TrackerRequest where
    decode obj = withObject obj $ 
           TrackerRequest <$> getValue "infoHash" 
           <*> getValue "peerId"
           <*> getValue "ip" 
           <*> getValue "uploaded"
           <*> getValue "downloaded"
           <*> getValue "left"
           <*> getValue "compact"
           <*> getValue "noPeerId"
           <*> getValue "event"
           <*> getValue "numwant"
           <*> getValue "key"
           <*> getValue "trackerId"

data TrackerResponse = TrackerResponse {
    interval :: Int,
    minInterval :: Maybe Int,
    _restrackerId :: Maybe TrackerId
}



instance DecodeBEncoding TrackerResponse where
    decode obj = withObject obj $ TrackerResponse
        <$> getValue "interval"
        <*> getValue "minInterval"
        <*> getValue "trackerId"
       
