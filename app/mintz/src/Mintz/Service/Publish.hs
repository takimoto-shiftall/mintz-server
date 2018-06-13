{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Mintz.Service.Publish (
    publishMessage
) where

import GHC.Generics
import Data.Proxy
import Data.IORef
import Data.String
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Database.Redis
import Data.Resource
import Mintz.Settings (REDIS, JTALK)
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk

data VoicePublish = VoicePublish { url :: String
                                 , kind :: String
                                 } deriving (Eq, Show, Generic)

instance ToJSON VoicePublish

publishMessage :: (With '[REDIS, JTALK])
               => String
               -> String
               -> IO ()
publishMessage message kind' = do
    jtalk <- readIORef $ contextOf @JTALK ?cxt
    path <- outputMP3 jtalk message

    case path of
        Just p -> do
            let url' = p
            (RedisPubSubContext conn) <- readIORef $ contextOf @REDIS ?cxt
            runRedis conn $ publish "mintz" $ B.toStrict (encode (VoicePublish url' kind'))
            return ()
        Nothing -> return ()