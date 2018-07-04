{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mintz.Resource.TypeTalk.Subscribe where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Safe
import Data.String (fromString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Network.WebSockets
import Network.WebSockets.Stream
import Network.Connection
import Network.Socket (PortNumber, withSocketsDo)
import Mintz.Resource.TypeTalk.Auth
import Mintz.Resource.TypeTalk.Model

data SubscriptionStatus = Finished
                        | Closed BL.ByteString
                        | Crashed SomeException
                        deriving (Show)

subscribe :: (WebSocketsData a)
          => Credential
          -> (a -> IO ())
          -> IO (MVar SubscriptionStatus, IO ())
subscribe cred f = do
    withSocketsDo $ runWithTLS "typetalk.com"
                               "/api/v1/streaming"
                               443
                               [ ("Authorization", fromString $ "Bearer " ++ access_token cred) ]
                               (subscribe' f)

subscribe' :: forall a. (WebSocketsData a)
           => (a -> IO ())
           -> ClientApp (MVar SubscriptionStatus, IO ())
subscribe' f = \conn -> do
    mvar <- newEmptyMVar :: IO (MVar SubscriptionStatus)

    let cleanup = \s -> do
            putMVar mvar $ case s of
                Left e -> case fromException e :: Maybe ConnectionException of
                            Just (CloseRequest _ m) -> Closed m
                            Just ce -> Crashed e
                            Nothing -> Crashed e
                Right _ -> Finished

    let closer = sendClose conn (T.pack "bye")

    (`forkFinally` cleanup) $ do
        forever $ do
            msg <- receiveData conn :: IO a
            f msg

    return (mvar, closer)

runWithTLS :: String -> String -> PortNumber -> Headers -> ClientApp a -> IO a
runWithTLS host path port headers app = do
    context <- initConnectionContext
    conn <- connectTo context $ ConnectionParams {
          connectionHostname = host
        , connectionPort = port
        , connectionUseSecure = Just tlsSettings
        , connectionUseSocks = Nothing
        }
    stream <- makeStream
                (Just <$> connectionGetChunk conn)
                (maybe (return ()) (connectionPut conn . BL.toStrict))
    runClientWithStream stream host path defaultConnectionOptions headers app

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = False
    }

data SubscriptionMessage = PostMessage Sub'Topic Sub'Post
                         | UnknownMessage String

instance FromJSON SubscriptionMessage where
    parseJSON = withObject "SubscriptionMessage" $ \v -> do
        (v .: "type" :: Parser String) >>= \t ->
            case t of
                "postMessage" -> (v .: "data" :: Parser Value) >>= parsePostMessage
                _ -> return $ UnknownMessage t
        where
            parsePostMessage :: Value -> Parser SubscriptionMessage
            parsePostMessage = withObject "PostMessage" $ \d -> PostMessage
                <$> d .: "topic"
                <*> d .: "post"
