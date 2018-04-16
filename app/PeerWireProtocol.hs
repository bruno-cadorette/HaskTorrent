module PeerWireProtocol where

import Data.Serialize.Get
import Data.ByteString

data ClientConnectionState = ClientConnectionState {
        _localState :: ConnectionState,
        _remoteState :: ConnectionState
    }

data ConnectionState  = ConnectionState {
        _choking :: Bool,
        _interested :: Bool
    }

data Peer = Peer {
        _clientConnectionState :: ClientConnectionState,
        _pieces :: [Bool]
    }

data MessageType = 
      KeepAlive
    | Choke
    | Unchoke
    | Interested
    | NotInterested
    | Have Int
    | BitField [Bool]
    | Request Int Int Int
    | Piece Int Int ByteString
    | Cancel Int Int Int
    | Port Int

defaultConnectionState = ConnectionState True False

handshake = do 
    pstrlen <- getWord8
    pstr <- getBytes (fromIntegral pstrlen) 
    reserved <- getBytes 8
    info_hash <- getBytes 20
    peer_id <- getBytes 20
    return peer_id

-- Need length of block and number of pieces in the ReaderT
getMessageType peer n =
    case n of
        0 -> return Choke
        1 -> return Unchoke
        2 -> return Interested
        3 -> return NotInterested
        4 -> (Have . fromIntegral) <$>  getWord8
        5 -> return $ BitField []
        6 -> do
            index <- fromIntegral <$> getWord32be
            begin <- fromIntegral <$> getWord32be
            length <-fromIntegral <$>  getWord32be
            return $ Request index begin length
        7 ->  undefined
        8 -> do
            index <- fromIntegral <$> getWord32be
            begin <- fromIntegral <$> getWord32be
            length <- fromIntegral <$>  getWord32be
            return $ Request index begin length
        9 -> (Port . fromIntegral) <$> getInt32be 

deserializeMessage :: Peer -> Get MessageType
deserializeMessage peer = do
    n <- getMaybeOf getWord8
    case n of
        Just id -> getMessageType peer id
        Nothing -> return KeepAlive