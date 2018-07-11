{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Mintz.Settings where

import GHC.Generics
import qualified Data.Map as M
import Data.Aeson
import Network.Wai
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Ext.Servant.Context
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk
import Mintz.Resource.TypeTalk
import Mintz.Resource.Wechime

newtype Database = Database PostgreSQL deriving (DBSettings)

db = Database (PostgreSQL "postgresql://postgres:postgres@172.16.147.59:15432/mintz" 10)
--db = Database (PostgreSQL "postgresql://postgres:postgres@172.27.146.49:15432/mintz" 10)

type DB = DBContext Database
type REDIS = RedisPubSubContext
type JTALK = OpenJTalkContext
type CHATBOT = TypeTalkBotContext
type WECHIME = WechimeContext

type SiteKeys = '[]

type SiteContext = RequestContext SiteKeys (Refs '[DB, REDIS, JTALK, CHATBOT, WECHIME, Logger])

(@@) :: (Contexts (Refs '[DB, REDIS, JTALK, CHATBOT, WECHIME, Logger]) -> a)
     -> SiteContext
     -> a
(@@) f (RequestContext _ cs _) = f cs

publicPath = "/public"

data DatabaseSettings = DatabaseSettings {
      dsn :: String
    , max_connections :: Int
    } deriving (Generic)

data LinkSettings = LinkSettings {
      icon_dir :: String
    , icon_url :: String
    , audio_url :: String
    , default_icon :: String
    , default_audio :: String
    } deriving (Generic)

data TypeTalkSettings = TypeTalkSettings {
      topic_id :: Int
    , token :: String
    } deriving (Generic)

data CrossDomainSettings = CrossDomainSettings {
      origin :: String
    } deriving (Generic)

data VoiceProperties = VoiceProperties {
      path :: String
    , description :: String
    } deriving (Generic)

data JTalkSettings = JTalkSettings {
      dict_dir :: String
    , voice_dir :: String
    , out_dir :: String
    , voices :: M.Map String VoiceProperties
    } deriving (Generic)

data WechimeSettings = WechimeSettings {
      gpio_dir :: String
    , gpio_chime1 :: Int
    , gpio_chime2 :: Int
    , gpio_chime12 :: Int
    } deriving (Generic)

data AppSettings = AppSettings {
      database :: DatabaseSettings
    , link :: LinkSettings
    , type_talk :: TypeTalkSettings
    , cross_domain :: CrossDomainSettings
    , open_jtalk :: JTalkSettings
    , wechime :: WechimeSettings
    } deriving (Generic)

instance FromJSON DatabaseSettings
instance FromJSON LinkSettings
instance FromJSON TypeTalkSettings
instance FromJSON CrossDomainSettings
instance FromJSON VoiceProperties
instance FromJSON JTalkSettings
instance FromJSON WechimeSettings
instance {-# OVERLAPPING #-} FromJSON AppSettings

typeTalkBot :: AppSettings
            -> TypeTalkBot
typeTalkBot (AppSettings { type_talk = TypeTalkSettings {..}, ..}) = TypeTalkBot token topic_id

openJTalk :: AppSettings
          -> OpenJTalk
openJTalk (AppSettings { open_jtalk, .. }) = OpenJTalk { dictDir = dict_dir open_jtalk
                                                       , voiceDir = voice_dir open_jtalk
                                                       , outDir = out_dir open_jtalk
                                                       }
