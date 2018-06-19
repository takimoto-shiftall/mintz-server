{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

module Mintz.Service.Publish (
    publishMessage
) where

import GHC.Generics
import Data.Proxy
import Data.IORef
import Data.String
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Control.Lens
import Data.Extensible
import Database.Redis
import Data.Model.Graph
import Data.Resource
import Database.ORM
import Mintz.Settings (DB, REDIS, JTALK, CHATBOT)
import Mintz.Model.Models
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk
import Mintz.Resource.TypeTalk

data VoicePublish = VoicePublish { url :: String
                                 , kind :: String
                                 } deriving (Eq, Show, Generic)

instance ToJSON VoicePublish

type PublishPerson = "person" :## Record (Person' ^@ '["id", "notifications"])
type PublishGraph = Graph PublishPerson

publishMessage :: (With '[DB, REDIS, JTALK, CHATBOT])
               => String
               -> String
               -> Maybe String
               -> String
               -> Maybe [Int]
               -> IO ()
publishMessage message kind' voice channel persons = do
    accounts <- case persons of
        Nothing -> return []
        Just ps -> do
            with @'[DB] $ do
                graph <- selectNodes (Proxy :: Proxy PublishGraph)
                                     (Proxy :: Proxy PublishPerson)
                                     ((=@?) @PublishPerson "id" ps)
                                     (orderBy @PublishPerson "id" ASC)
                                     Nothing
                return $ catMaybes $ map typeTalkName (values graph :: [PublishPerson])

    with @'[CHATBOT] $ postMessage message accounts

    jtalk <- readIORef $ contextOf @JTALK ?cxt
    path <- outputMP3 jtalk voice message

    case path of
        Just p -> do
            let url' = p
            (RedisPubSubContext conn) <- readIORef $ contextOf @REDIS ?cxt
            runRedis conn $ publish (fromString channel) $ B.toStrict (encode (VoicePublish url' kind'))
            return ()
        Nothing -> return ()