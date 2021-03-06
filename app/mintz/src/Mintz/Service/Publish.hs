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
  , publishSound
  , removeSound
) where

import GHC.Generics
import Control.Concurrent
import Control.Applicative
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
import System.Directory
import System.FilePath ((</>), (<.>))
import Control.Lens hiding ((:>), (<.>))
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
--
-- The name of audio file is the hash text of the message.
-- When the file already exists, it is just reused.
publishMessage :: (With '[DB, REDIS, JTALK, CHATBOT, WECHIME])
               => PublishEntry -- ^ Object containing information of the publishing.
               -> (String -> String) -- ^ Function converting path of generated audio file to accessible URL.
               -> String -- ^ URL of default audio file used when the generation fails.
               -> IO Bool -- ^ Denotes whether the audio is published to Redis.
publishMessage entry@(PublishEntry { kind = kind', extra = extra', .. }) formatUrl defaultAudio = do
    jtalk@(OpenJTalkContext jr) <- readIORef $ contextOf @JTALK ?cxt

    (exists, hash) <- with @'[DB] $ audioExists message voice (outDir jr)

    let voiceMessage = unwords $ lines message

    $(logQD) ?cxt $ "Does audio for " ++ hash ++ " exists? " ++ show exists

    path <- with @'[JTALK] $ execMP3 voice voiceMessage hash (not exists)

    -- Fetches TypeTalk accounts and logs.
    accounts <- with @'[DB] $ do
        ps <- listPersons persons
        createLog' entry hash ps
        return $ catMaybes $ map typeTalkName ps

    -- POST message to TypeTalk in another thread with mentioning accounts if any.
    when (not $ isJustSpeech entry) $ do
        (TypeTalkBotContext tt) <- readIORef $ contextOf @CHATBOT ?cxt
        ttr <- newIORef tt
        void $ forkIO $ void $ withContext @'[CHATBOT] (ttr `RCons` RNil) $ postMessage message accounts

    case chimesToRing entry of
        [] -> return ()
        chimes -> with @'[WECHIME] $ execChime chimes

    -- Publish to Redis when the audio file is generated.
    if isSilent entry
        then do
            logD $ "Silent mode specified, audio is not published to redis."
            return False
        else do
            case path of
                Just p -> do
                    let url = formatUrl p
                    (RedisPubSubContext conn) <- readIORef $ contextOf @REDIS ?cxt
                    void $ runRedis conn $ publish (fromString channel) $ BL.toStrict (encode (VoicePublish url kind' extra'))
                    return True
                Nothing -> do
                    logD $ "Failed to generate audio file."
                    return False

runWechime :: (With '[WECHIME])
           => [Chime]
           -> IO ()
runWechime chimes = execChime chimes

publishSound :: (With '[REDIS])
             => FilePath
             -> String
             -> (String -> String)
             -> BL.ByteString
             -> IO ()
publishSound dir name formatUrl sound = do
    let path = dir </> name <.> "mp3"
    BL.writeFile path sound
    let url = formatUrl path
    -- TODO: What channel and kind to use?
    publishRedis' "mintz" "sound" Nothing url

removeSound :: FilePath
            -> String
            -> IO ()
removeSound dir name = do
    let path = dir </> name <.> "mp3"
    doesPathExist path >>= flip when (removeFile path)

-- ----------------------------------------------------------------
-- Sub functions used in publishMessage.
-- ----------------------------------------------------------------

listPersons :: (With '[DB])
            => [Int]
            -> IO [Person]
listPersons [] = return []
listPersons ids = do
    graph <- selectNodes (Proxy :: Proxy (Graph Person))
                         (Proxy :: Proxy Person)
                         ((=@?) @Person "id" ids)
                         (orderBy @Person "id" ASC)
                         Nothing
    return (values graph :: [Person])

type AudioHashGraph = Graph PublishLog

audioExists :: (With '[DB])
            => String
            -> Maybe String
            -> FilePath
            -> IO (Bool, String)
audioExists message voice dir = do
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

publishRedis' :: (With '[REDIS])
              => String
              -> String
              -> Maybe Object
              -> String
              -> IO ()
publishRedis' channel kind' extra' url = do
    (RedisPubSubContext conn) <- readIORef $ contextOf @REDIS ?cxt
    void $ runRedis conn $ publish (fromString channel) $ BL.toStrict (encode (VoicePublish url kind' extra'))

-- ----------------------------------------------------------------
-- Functions to get flags to control publishing operation.
-- ----------------------------------------------------------------

chimesToRing :: PublishEntry
             -> [Chime]
chimesToRing (PublishEntry { kind = kind', extra = extra' }) = maybe [] id $ byExtra <|> byKind
    where
        toChime :: Int -> [Chime]
        toChime 1 = [Chime1]
        toChime 2 = [Chime2]
        toChime _ = []

        byExtra :: Maybe [Chime]
        byExtra = extra' >>= \ex -> case parse (flip parseField $ "wechime") ex of
            Data.Aeson.Error e -> Nothing
            Data.Aeson.Success cs -> Just $ concat $ map toChime cs

        byKind :: Maybe [Chime]
        byKind 
            | kind' `elem` ["guest", "deliverer", "visitor"] = Just [Chime1, Chime2]
            | otherwise = Nothing

isSilent :: PublishEntry
         -> Bool
isSilent (PublishEntry { extra = Just extra' }) = case parse (flip parseField $ "silent") extra' of
    Data.Aeson.Error e -> False
    Data.Aeson.Success b -> b
isSilent _ = False

isJustSpeech :: PublishEntry
             -> Bool
isJustSpeech (PublishEntry { kind = kind' }) = kind' == "speech"