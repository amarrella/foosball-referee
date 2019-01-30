{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Web.FormUrlEncoded (FromForm)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (delete, nub)
import Data.Map hiding (delete)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

data IncomingMessage = IncomingMessage
  { channel_id :: String
  , user_id :: String
  , text :: String
  } deriving (Eq, Show, Generic)

instance FromForm IncomingMessage

newtype ChannelId = ChannelId String deriving (Eq, Show, Ord)

newtype UserId = UserId String deriving (Eq, Ord)

instance Show UserId where
  show (UserId x) = "@" ++ x

data Command = 
  Add ChannelId UserId |
  Remove ChannelId UserId |
  ForceNew ChannelId UserId | 
  Status ChannelId deriving (Eq, Show)

fromMessage :: IncomingMessage -> Maybe Command
fromMessage (IncomingMessage c u "play") = Just $ Add (ChannelId c) (UserId u)
fromMessage (IncomingMessage c u "give up") = Just $ Remove (ChannelId c) (UserId u)
fromMessage (IncomingMessage c u "force new") = Just $ ForceNew (ChannelId c) (UserId u)
fromMessage (IncomingMessage c _ "status") = Just $ Status (ChannelId c)
fromMessage _ = Nothing

data ResponseType = InChannel | Ephemeral deriving (Eq, Show)

instance ToJSON ResponseType where 
  toJSON InChannel = String "in_channel"
  toJSON Ephemeral = String "ephemeral"

data MessageResponse = MessageResponse
  {
    gameState :: Either String ChannelPlayers
  , responseType :: ResponseType
  } deriving Show

instance ToJSON MessageResponse where
  toJSON (MessageResponse s t) = object ["text" .= s, "response_type" .= t]

type API = "message" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] MessageResponse

data ChannelPlayers = 
  Zero | One UserId | Two UserId UserId | Three UserId UserId UserId | Four UserId UserId UserId UserId 
  deriving (Eq, Show)

instance ToJSON ChannelPlayers where
  toJSON Zero = String "No game is on"
  toJSON s@Four{} = String $ T.concat ["Game on! Players ", T.pack . show $ fmap show (players s)]
  toJSON s = String $ T.concat ["Current players: ", T.pack . show $ fmap show (players s)]

players :: ChannelPlayers -> [UserId]
players Zero = []
players (One p1) = [p1]
players (Two p1 p2) = [p1, p2]
players (Three p1 p2 p3) = [p1, p2, p3]
players (Four p1 p2 p3 p4) = [p1, p2, p3, p4]

toPlayers :: [UserId] -> ChannelPlayers
toPlayers [] = Zero
toPlayers [p1] = One p1
toPlayers [p1, p2] = Two p1 p2
toPlayers [p1, p2, p3] = Three p1 p2 p3
toPlayers [p1, p2, p3, p4] = Four p1 p2 p3 p4
toPlayers (p1:p2:p3:p4:ps) = toPlayers ps

instance Semigroup ChannelPlayers where
  x <> y = toPlayers $ nub ((players x) ++ (players y))

type GameState = Map ChannelId ChannelPlayers

removePlayer :: ChannelPlayers -> UserId -> ChannelPlayers
removePlayer s u = toPlayers $ delete u (players s)

startApp :: IO ()
startApp = do 
  gameState <- atomically $ newTVar (empty :: GameState)
  run 8080 (app gameState)

app :: TVar GameState -> Application
app s = serve api (server s)

api :: Proxy API
api = Proxy

messageRoute :: TVar GameState -> IncomingMessage -> Handler MessageResponse
messageRoute state msg = liftIO $ do
  let command = fromMessage msg
  case command of
    Nothing -> return $ MessageResponse (Left $ "Invalid command " ++ show (text msg)) Ephemeral
    Just (Add c p) -> atomically $ do
      s <- readTVar state
      let oldChannelState = fromMaybe Zero (s !? c)
      let newChannelState = oldChannelState <> One p
      let newState = insert c newChannelState s
      writeTVar state newState
      let response = if newChannelState == oldChannelState 
                     then MessageResponse (Left $ "Player " ++ show p ++ " already in the game.") Ephemeral
                     else MessageResponse (Right newChannelState) InChannel
      return response
    Just (Remove c p) -> atomically $ do
      s <- readTVar state
      let oldChannelState = fromMaybe Zero (s !? c)
      let newChannelState = oldChannelState `removePlayer` p
      let newState = insert c newChannelState s
      writeTVar state newState
      return $ MessageResponse (Right newChannelState) InChannel
    Just (ForceNew c p) -> atomically $ do
      s <- readTVar state
      let newChannelState = One p
      let newState = insert c newChannelState s
      writeTVar state newState
      return $ MessageResponse (Right newChannelState) InChannel
    Just (Status c) -> atomically $ readTVar state
      >>= \s -> return $ MessageResponse (Right (fromMaybe Zero (s !? c))) InChannel

server :: TVar GameState -> Server API
server = messageRoute 
