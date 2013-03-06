{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Web.Tinder.Types
    ( CampfireEnv(..)
    , Message(..)
    , pathToBS
    , Room(..)
    , RoomId
    , Speak(..)
    , subdomainToUrl
    ) where

import           Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.UTF8 (fromString)
import           Network.Http.Client (URL)

data CampfireEnv = CampfireEnv
    { cfSubdomain :: ByteString
    , cfToken :: ByteString
    } deriving (Show)

subdomainToUrl :: ByteString -> URL
subdomainToUrl sub = B.concat ["https://", sub, ".campfirenow.com"]

data Message = TextMessage ByteString

instance ToJSON Message where
    toJSON (TextMessage body) = object ["message" .= message]
          where
        message = object ["body" .= body, "type" .= textMessage]
        textMessage = "TextMessage" :: ByteString

type RoomId = Integer

class CampfirePath a where
    pathToBS :: a -> ByteString

data Room = Room RoomId

instance CampfirePath Room where
    pathToBS (Room roomId) = B.concat ["/room/", fromString . show $ roomId]


data Speak = Speak Room

instance CampfirePath Speak where
    pathToBS (Speak room) = B.concat [pathToBS room, "/speak"]

