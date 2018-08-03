{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Mintz.Service.Publish (
    PublishEntry(..)
  , publishMessage
  , runWechime
) where

import GHC.Generics
import Control.Concurrent
import Control.Monad
import Data.Proxy
import Data.IORef
import Data.String
import Data.Time
import Data.Maybe (catMaybes, maybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Aeson.Types
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
import Control.Lens hiding ((:>))
import Data.Extensible
import Database.Redis
import Data.Model.Graph
import Data.Resource
import Database.ORM
import Mintz.Settings (DB, REDIS, JTALK, CHATBOT, WECHIME)
import Mintz.Model.Models
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk
import Mintz.Resource.TypeTalk
import Mintz.Resource.Wechime
import Mintz.Service.Log (createLog)
import Debug.Trace

-- | Contains values used to publish a message.
data PublishEntry = PublishEntry { message :: String -- ^ Message text.
                                 , kind :: String -- ^ Message kind.
                                 , voice :: Maybe String -- ^ Voice type. Default voice is used if this is @Nothing@.
                                 , channel :: String -- ^ Channel name.
                                 , persons :: [Int] -- ^ Person IDs to mention in TypeTalk.
                                 , extra :: Maybe Object -- ^ Extra values passed to Redis. This also controls several operations.
                                 } deriving (Show, Generic)

instance ToJSON PublishEntry

-- | Defines format of JSON data published to Redis.
data VoicePublish = VoicePublish { audio :: String -- ^ Audio file URL.
                                 , kind :: String -- ^ Message kind.
                                 , extra :: Maybe Object -- ^ Extra values which Redis clients interpret as their own way.
                                 } deriving (Eq, Show, Generic)

instance ToJSON VoicePublish

-- | Creates audio file pronouncing a message and publish its URL to Redis.
--
-- In addition, this function executes
-- - Posting a message to TypeTalk
-- - Ringing Wechime.
-- - Logging.
publishMessage :: (With '[DB, REDIS, JTALK, CHATBOT, WECHIME])
               => PublishEntry -- ^ Object containing information of the publishing.
               -> (String -> String) -- ^ Function converting path of generated audio file to accessible URL.
               -> String -- ^ URL of default audio file used when the generation fails.
               -> IO Bool -- ^ Always True.
publishMessage (PublishEntry { message = "", .. }) _ _ = return False
publishMessage entry@(PublishEntry { kind = kind', extra = extra', .. }) formatUrl defaultAudio = do
    accounts <- case persons of
        [] -> return []
        _ -> with @'[DB] $ do
                graph <- selectNodes (Proxy :: Proxy (Graph Person))
                                     (Proxy :: Proxy Person)
                                     ((=@?) @Person "id" persons)
                                     (orderBy @Person "id" ASC)
                                     Nothing
                return (values graph :: [Person])

    -- Generate audio file whose name is the hash text of the message.
    -- When the file already exists, just returns its path.
    jtalk@(OpenJTalkContext jr) <- readIORef $ contextOf @JTALK ?cxt

    (exists, hash) <- with @'[DB] $ audioExists message voice (outDir jr)

    let voiceMessage = unwords $ lines message

    $(logQD) ?cxt $ "Does audio for " ++ hash ++ " exists? " ++ show exists

    path <- with @'[JTALK] $ execMP3 voice voiceMessage hash (not exists)

    -- Create log of publishing message.
    with @'[DB] $ createLog' entry hash accounts

    -- FIXME 一時的にkindで区別
    when (not $ kind' == "speech") $ do
        (TypeTalkBotContext tt) <- readIORef $ contextOf @CHATBOT ?cxt
        ttr <- newIORef tt

        -- POST message to TypeTalk in another thread with mentioning accounts if any.
        forkIO $ do
            withContext @'[CHATBOT] (ttr `RCons` RNil) $ postMessage message (catMaybes $ map typeTalkName accounts)
            return ()
        return ()

    case extra' >>= findWechimeParams of
        Nothing -> do
            -- FIXME 一時的に直書き
            if kind' `elem` ["guest", "deliverer", "visitor"]
                then with @'[WECHIME] $ execChime [Chime1, Chime2]
                else return ()
        Just chimes -> with @'[WECHIME] $ execChime chimes

    -- Publish to Redis according to whether valid audio file is available.
    if maybe True not (extra' >>= isSilent)
        then do
            let url = maybe defaultAudio formatUrl path

            (RedisPubSubContext conn) <- readIORef $ contextOf @REDIS ?cxt
            runRedis conn $ publish (fromString channel) $ BL.toStrict (encode (VoicePublish url kind' extra'))
            return ()
        else
            logD $ "Silent mode specified, audio is not published to redis."

    return True

-- TODO 行ロックが必要
type AudioHashGraph = Graph PublishLog

audioExists :: (With '[DB])
            => String
            -> Maybe String
            -> FilePath
            -> IO (Bool, String)
audioExists message voice dir = do
    --let audioHash = show $ hashWith SHA1 (fromString (message ++ voice') :: B.ByteString)
    let audioHash = UTF8.toString $ B16.encode $ SHA1.hash (fromString $ message ++ voice')

    $(logQD) ?cxt $ "Audio hash: " ++ audioHash

    graph <- selectNodes (Proxy :: Proxy AudioHashGraph)
                         (Proxy :: Proxy PublishLog)
                         ((==?) @PublishLog "audio_hash" audioHash)
                         (orderBy @PublishLog "id" DESC)
                         (Just (1, 0))

    $(logQD) ?cxt $ "Got record matching hash: " ++ show (length (values graph :: [PublishLog]))

    return (any match (values graph :: [PublishLog]), audioHash)
    where
        voice' = maybe "" id voice

        match :: PublishLog -> Bool
        match pl = let r = getRecord pl
                   in view #message r == message && view #voice r == voice'

runWechime :: (With '[WECHIME])
           => [Chime]
           -> IO ()
runWechime chimes = execChime chimes

createLog' :: (With '[DB])
           => PublishEntry
           -> String
           -> [Person]
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
                   ) :: (=+)PublishLog
    graph <- createLog pl ps
    return ()

findWechimeParams :: Object
                  -> Maybe [Chime]
findWechimeParams obj = case parse (flip parseField $ "wechime") obj of
    Data.Aeson.Error e -> Nothing
    Data.Aeson.Success cs -> Just $ concat $ map toChime cs
    where
        toChime :: Int -> [Chime]
        toChime 1 = [Chime1]
        toChime 2 = [Chime2]
        toChime _ = []

isSilent :: Object
         -> Maybe Bool
isSilent obj = case parse (flip parseField $ "silent") obj of
    Data.Aeson.Error e -> Nothing
    Data.Aeson.Success b -> Just b