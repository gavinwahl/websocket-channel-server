{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.List
import Control.Exception (finally)
import Data.Maybe (fromJust)

import qualified Network.WebSockets as WS
import qualified Data.Text          as T
import qualified Data.ByteString    as BS
import qualified Data.Map           as Map

import Data.Aeson

type ConnectionID = Integer

type ChannelName = BS.ByteString

data Message = Welcome [Connection] Connection |
               PeerMessage Connection T.Text |
               Join Connection |
               Disconnect Connection

instance ToJSON Message where
  toJSON (Welcome members newCon) = object [
      "type" .= ("welcome" :: T.Text)
    , "members" .= members
    , "yourid" .= newCon
    ]
  toJSON (PeerMessage connection message) = object [
      "type" .= ("peermessage" :: T.Text)
    , "message" .= message
    , "from" .= connection
    ]
  toJSON (Join connection) = object [
      "type" .= ("join" :: T.Text)
    , "id" .= connection
    ]
  toJSON (Disconnect connection) = object [
      "type" .= ("disconnect" :: T.Text)
    , "id" .= connection
    ]

instance ToJSON Connection where
  toJSON Connection { conID = c } = toJSON c

data Connection = Connection {
  conHandle :: WS.Connection,
  conID :: ConnectionID
}

instance Eq Connection where
  Connection {conID = a} == Connection {conID = b} = a == b

data Channel = Channel {
  nextID :: Integer,
  connections :: [Connection]
}

data ServerState = ServerState {
  channels :: Map.Map BS.ByteString Channel
}

newServerState :: ServerState
newServerState = ServerState {channels=Map.empty}

newChannel :: Channel
newChannel = Channel {nextID=1, connections=[]}

getChannelByName :: ChannelName -> ServerState -> Channel
getChannelByName channelName state = fromJust $ Map.lookup channelName $ channels state

addConnection :: Channel -> Connection -> Channel
addConnection channel connection = channel {
    nextID=nextID channel + 1,
    connections=connection : connections channel
  }

removeConnection :: ServerState -> ChannelName -> Connection -> ServerState
removeConnection state channelName connection = state {
      channels=if null $ connections channel'
        then Map.delete channelName allChannels
        else Map.insert channelName channel' allChannels
    }
  where
    allChannels = channels state
    channel = getChannelByName channelName state
    channel' = channel { connections=delete connection $ connections channel }

acceptConnection :: MVar ServerState -> WS.ServerApp
acceptConnection serverState pending = do
    print $ WS.pendingRequest pending
    let channelName = WS.requestPath $ WS.pendingRequest pending
    handle <- WS.acceptRequest pending
    connection <- modifyMVar serverState (\state ->
      let channel = Map.findWithDefault newChannel channelName (channels state)
          connection = Connection {conHandle=handle, conID=nextID channel}
          channel' = addConnection channel connection
      in return (state {channels=Map.insert channelName channel' (channels state)}, connection))
    application serverState channelName connection

application :: MVar ServerState -> ChannelName -> Connection -> IO ()
application serverState channelName connection = do
    withMVar serverState (\state ->
      let channel = getChannelByName channelName state
      in sendMessage connection $ Welcome (connections channel) connection)
    broadcast $ Join connection
    forever (talk serverState connection broadcast) `finally` disconnect
  where 
    handle = conHandle connection
    sendMessage con message = WS.sendTextData (conHandle con) $ encode message
    broadcast message = do
      withMVar serverState (\state ->
        let channel = getChannelByName channelName state
        in forM_ (connections channel) (\c ->
            when (c /= connection) $ sendMessage c message))
    disconnect = do
      modifyMVar_ serverState (\state -> return $ removeConnection state channelName connection)
      broadcast $ Disconnect connection
    
talk :: MVar ServerState -> Connection -> (Message -> IO ()) -> IO ()
talk serverState myConnection broadcast = do
    packet <- WS.receiveData handle 
    broadcast $ PeerMessage myConnection packet
  where
    handle = conHandle myConnection
  
main :: IO ()
main = do
  serverState <- newMVar newServerState
  WS.runServer "0.0.0.0" 12345 $ acceptConnection serverState
