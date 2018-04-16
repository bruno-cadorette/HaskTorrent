{-# LANGUAGE OverloadedStrings #-}

module Networking where

import Network.Socket hiding (send, recv)
import qualified Data.ByteString as B
import Network.Socket.ByteString (send, recv)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad

data Connection = Connection {_socket :: Socket, _address :: SockAddr, _threadId :: ThreadId} deriving (Show)
data Message =  NewPeer (TMVar ()) | NewMessage B.ByteString


magicPort :: PortNumber
magicPort = 15500

newSocket = socket AF_INET Stream 0
mySocketAddr = do 
    let hint = defaultHints {
        addrFlags = [AI_PASSIVE],
        addrSocketType = Stream
    }
    fmap (addrAddress . head) $ getAddrInfo (Just hint) Nothing (Just "15500")


receiveMessages channel socket stopVar = do
    result <- race (atomically $ takeTMVar stopVar) (recv socket 128)
    case result of
        Right message -> do 
            if B.null message then cleanup
            else do
                atomically $ writeTChan channel (NewMessage message)
                putStrLn ("Server: " ++ show message)
                receiveMessages channel socket stopVar
        Left () -> cleanup
    where 
        cleanup = do
            putStrLn "closing the socket"
            close socket

serve :: TChan Message -> IO void
serve channel = do
    listeningSocket <- newSocket
    addr <- mySocketAddr
    bind listeningSocket addr
    listen listeningSocket 10
    forever $ do
        (connectedSocket, address) <- accept listeningSocket
        stopVar <- newEmptyTMVarIO
        atomically $ writeTChan channel (NewPeer stopVar)
        forkIO (receiveMessages channel connectedSocket stopVar)


sendTest = do
    listeningSocket <- newSocket
    addr <- mySocketAddr
    connect listeningSocket addr
    send listeningSocket "allo"
    print "sent allo"
    threadDelay 1000000
    send listeningSocket "message 2"
    print "sent message 2"
    threadDelay 1000000
    send listeningSocket "le starbuck c'est cool"
    threadDelay 1000000
    send listeningSocket "bye" 
    threadDelay 1000000
    close listeningSocket

stopProgram channel = do
    x <- readTChan channel
    case x of 
        NewPeer mvar -> putTMVar mvar ()
        _ -> return ()

mainNetwork = do
    var <- newEmptyTMVarIO
    channel <- newTChanIO
    forkIO (atomically $ stopProgram channel)
    forkIO (serve channel)
    threadDelay 1000000
    forkIO $ sendTest 
    atomically $ readTMVar var
