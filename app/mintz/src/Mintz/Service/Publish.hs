{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Mintz.Service.Publish (
    PublishEntry(..)
  , publishMessage
) where

import GHC.Generics
import Control.Concurrent
import Data.Proxy
import Data.IORef
import Data.String
import Data.Time
import Data.Maybe (catMaybes, maybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Aeson
import Crypto.Hash
import Control.Lens hiding ((:>))
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
import Mintz.Service.Log (createLog)

data PublishEntry = PublishEntry { message :: String
                                 , kind :: String
                                 , voice :: Maybe String
                                 , channel :: String
                                 , persons :: [Int]
                                 , extra :: Maybe Object
                                 } deriving (Show, Generic)

instance ToJSON PublishEntry

data VoicePublish = VoicePublish { audio :: String
                                 , kind :: String
                                 , extra :: Maybe Object
                                 } deriving (Eq, Show, Generic)

instance ToJSON VoicePublish

type PublishGraph = Graph Person'S

publishMessage :: (With '[DB, REDIS, JTALK, CHATBOT])
               => PublishEntry
               -> (String -> String)
               -> String
               -> IO Bool
publishMessage entry@(PublishEntry { kind = kind', extra = extra', .. }) formatUrl defaultAudio = do
    accounts <- case persons of
        [] -> return []
        _ -> with @'[DB] $ do
                graph <- selectNodes (Proxy :: Proxy PublishGraph)
                                     (Proxy :: Proxy Person'S)
                                     ((=@?) @Person'S "id" persons)
                                     (orderBy @Person'S "id" ASC)
                                     Nothing
                return (values graph :: [Person'S])

    -- Generate audio file whose name is the hash text of the message.
    -- When the file already exists, just returns its path.
    jtalk@(OpenJTalkContext jr) <- readIORef $ contextOf @JTALK ?cxt

    (exists, hash) <- with @'[DB] $ audioExists message voice (outDir jr)

    path <- with @'[JTALK] $ execMP3 voice message hash (not exists)

    -- Create log of publishing message.
    with @'[DB] $ createLog' entry hash accounts

    -- FIXME 一時的にkindで区別
    if not (kind' == "speech")
        then do
            (TypeTalkBotContext tt) <- readIORef $ contextOf @CHATBOT ?cxt
            ttr <- newIORef tt

            -- POST message to TypeTalk in another thread with mentioning accounts if any.
            forkIO $ do
                withContext @'[CHATBOT] (ttr `RCons` RNil) $ postMessage message (catMaybes $ map typeTalkName accounts)
                return ()
            return ()
        else
            return ()

    -- Publish to Redis according to whether valid audio file is available.
    let url = maybe defaultAudio formatUrl path

    (RedisPubSubContext conn) <- readIORef $ contextOf @REDIS ?cxt
    runRedis conn $ publish (fromString channel) $ BL.toStrict (encode (VoicePublish url kind' extra'))

    return True

-- TODO 行ロックが必要
type AudioHashGraph = Graph PublishLog

audioExists :: (With '[DB])
            => String
            -> Maybe String
            -> FilePath
            -> IO (Bool, String)
audioExists message voice dir = do
    let audioHash = show $ hashWith SHA1 (fromString (message ++ voice') :: B.ByteString)

    graph <- selectNodes (Proxy :: Proxy AudioHashGraph)
                         (Proxy :: Proxy PublishLog)
                         ((==?) @PublishLog "audio_hash" audioHash)
                         (orderBy @PublishLog "id" DESC)
                         (Just (1, 0))

    return (any match (values graph :: [PublishLog]), audioHash)
    where
        voice' = maybe "" id voice

        match :: PublishLog -> Bool
        match pl = let r = getRecord pl
                   in view #message r == message && view #voice r == voice'

createLog' :: (With '[DB])
           => PublishEntry
           -> String
           -> [Person'S]
           -> IO ()
createLog' (PublishEntry { kind = kind', .. }) hash ps = do
    now <- getCurrentTime
    let pl = Model ( #id @= 0
                  <: #kind @= kind'
                  <: #channel @= channel
                  <: #message @= message
                  <: #voice @= maybe "" id voice
                  <: #audio_hash @= hash
                  <: #published_at @= now
                  <: emptyRecord
                   ) :: PublishLog
    graph <- createLog pl ps
    return ()