{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Mintz.Resource.TypeTalk.Subscribe where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Safe
import Data.IORef
import Data.String (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Network.WebSockets
import Network.WebSockets.Stream
import Network.Connection
import Network.Socket (PortNumber, withSocketsDo)
import Data.Resource
import Mintz.Resource.TypeTalk.Auth
import Mintz.Resource.TypeTalk.Model
import Mintz.Resource.TypeTalk.Resource

data SubscriptionStatus = Finished
                        | Closed BL.ByteString
                        | Crashed SomeException
                        deriving (Show)

data TypeTalkSubscription = TypeTalkSubscription { clientId :: String
                                                 , clientSecret :: String
                                                 }

instance Resource TypeTalkSubscription where
    type ContextType TypeTalkSubscription = SubscriptionContext

    newContext ref = liftIO $ do
        TypeTalkSubscription { .. } <- readIORef ref
        cred <- getCredential (fromString clientId) (fromString clientSecret)
        newIORef $ SubscriptionContext cred

data SubscriptionContext = SubscriptionContext { credential :: Credential }

instance ResourceContext SubscriptionContext where
    type ResourceType SubscriptionContext = TypeTalkSubscription

    closeContext c _ = return c
    execContext cr f = f

forkSubscription :: (With '[SubscriptionContext], WebSocketsData a)
                 => ((With '[SubscriptionContext]) => a -> IO ())
                 -> ((With '[SubscriptionContext]) => IO ())
                 -> IO (MVar SubscriptionStatus, IO ())
forkSubscription f pong = do
    SubscriptionContext {..} <- readIORef $ contextOf @SubscriptionContext ?cxt
    subscribe credential f pong

subscribe :: (With '[SubscriptionContext], WebSocketsData a)
          => Credential
          -> ((With '[SubscriptionContext]) => a -> IO ())
          -> ((With '[SubscriptionContext]) => IO ())
          -> IO (MVar SubscriptionStatus, IO ())
subscribe cred f pong = do
    let pong' = with @'[SubscriptionContext] pong
    withSocketsDo $ runWithTLS "typetalk.com"
                               "/api/v1/streaming"
                               443
                               [ ("Authorization", fromString $ "Bearer " ++ access_token cred) ]
                               pong'
                               (subscribe' f)

subscribe' :: forall a. (With '[SubscriptionContext], WebSocketsData a)
           => ((With '[SubscriptionContext]) => a -> IO ())
           -> ClientApp (MVar SubscriptionStatus, IO ())
subscribe' f = \conn -> do
    mvar <- newEmptyMVar :: IO (MVar SubscriptionStatus)

    let cleanup = \s -> do
            cause <- case s of
                Left e -> case fromException e :: Maybe ConnectionException of
                    Just (CloseRequest _ m) -> do
                        $(logQI' typeTalkTag) ?cxt $ "Closing request was received."
                        return $ Closed m
                    Just ce -> do
                        $(logQE' typeTalkTag) ?cxt $ "Connection exception: " ++ show ce
                        return $ Crashed e
                    Nothing -> do
                        $(logQE' typeTalkTag) ?cxt $ "Unexpected exception: " ++ show e
                        return $ Crashed e
                Right _ -> do
                    $(logQI' typeTalkTag) ?cxt $ "Subscription was stopped."
                    return Finished

            putMVar mvar cause

    let closer = do
            $(logQI' typeTalkTag) ?cxt $ "Send closing request."
            sendClose conn (T.pack "bye")

    (`forkFinally` cleanup) $ do
        forever $ do
            msg <- receiveData conn :: IO a
            f msg `catch` \(e :: SomeException) -> do
                $(logQE' typeTalkTag) ?cxt $ show e

    forkPingThread conn 30

    return (mvar, closer)

runWithTLS :: String
           -> String
           -> PortNumber
           -> Headers
           -> IO ()
           -> ClientApp a
           -> IO a
runWithTLS host path port headers pong app = do
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
    let options = defaultConnectionOptions { connectionOnPong = pong }
    runClientWithStream stream host path options headers app

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
                "updateMessage" -> (v .: "data" :: Parser Value) >>= parsePostMessage
                _ -> return $ UnknownMessage t
        where
            parsePostMessage :: Value -> Parser SubscriptionMessage
            parsePostMessage = withObject "PostMessage" $ \d -> PostMessage
                <$> d .: "topic"
                <*> d .: "post"
