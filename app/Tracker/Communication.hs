module Tracker.Communication where

import Metainfo.File
import Tracker.Types
import Network.Wreq

newRequest :: MetainfoFile -> TrackerRequest
newRequest = undefined

request :: TrackerRequest -> IO ()
request = undefined
{-
header req = params [("info_hash", get (req.infoHash)),
                    ("peer_id", get (req.peerId)),
                    (ip, get (req.ip))]

-}
-- Maybe a -> (a -> b) -> [b]
