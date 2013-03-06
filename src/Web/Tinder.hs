{-# LANGUAGE OverloadedStrings #-}

module Web.Tinder
    ( cfRequest
    , cfGet
    , cfPost
    , speak
    ) where

-- import           Blaze.ByteString.Builder (Builder)
import           Data.Aeson (encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.UTF8 (fromString)
import           Data.Maybe (fromMaybe)
-- import qualified Data.Text as T
import           Network.Http.Client
import           OpenSSL (withOpenSSL)
import qualified System.IO.Streams as Streams

import Web.Tinder.Types
import Web.Tinder.Util

subdomainToUrl :: ByteString -> URL
subdomainToUrl sub = B.concat ["https://", sub, ".campfirenow.com"]

sslConnection :: URL -> (Connection -> IO a) -> IO a
sslConnection url f = withOpenSSL $ withConnection (putStrLn ("Connecting to " ++ (show url)) >> establishConnection url) f

speak :: CampfireEnv -> RoomId -> ByteString -> IO ()
speak creds roomId message = cfPost creds path body
  where
    path = pathToBS . Speak $ Room roomId
    body = toStrict . encode $ TextMessage message

cfGet :: CampfireEnv -> ByteString -> IO ()
cfGet creds path = cfRequest creds GET path Nothing

cfPost :: CampfireEnv -> ByteString -> ByteString -> IO ()
cfPost creds path body = cfRequest creds POST path $ Just body

cfRequest :: CampfireEnv -> Method -> ByteString -> Maybe ByteString -> IO ()
cfRequest creds method path mbody = sslConnection url $ \conn -> do
    q <- buildRequest conn $ do
        http method path
        setAccept "application/json"
        setHeader "Content-type" "application/json"
        setAuthorizationBasic t "X"
        setContentLength bodyLength

    putStrLn $ show q

    bodyStream <- bodyStreamM
    sendRequest conn q (inputStreamBody bodyStream)
    receiveResponse conn (\p i -> do
        putStrLn $ show p
        x <- Streams.read i
        C.putStrLn $ fromMaybe "" x)
  where
    token = cfToken creds
    url = subdomainToUrl $ cfSubdomain creds
    body = fromMaybe "" mbody
    bodyLength = B.length body
    bodyStreamM = Streams.fromGenerator $ Streams.yield body
