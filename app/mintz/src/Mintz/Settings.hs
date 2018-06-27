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

newtype Database = Database PostgreSQL deriving (DBSettings)

db = Database (PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/mintz" 10)

type DB = DBContext Database
type REDIS = RedisPubSubContext
type JTALK = OpenJTalkContext
type CHATBOT = TypeTalkBotContext

type SiteKeys = '[]

type SiteContext = RequestContext SiteKeys (Refs '[DB, REDIS, JTALK, CHATBOT, Logger])

(@@) :: (Contexts (Refs '[DB, REDIS, JTALK, CHATBOT, Logger]) -> a)
     -> SiteContext
     -> a
(@@) f (RequestContext _ cs _) = f cs

publicPath = "/public"

data LinkSettings = LinkSettings {
      icon_url :: String
    , audio_url :: String
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

data AppSettings = AppSettings {
      link :: LinkSettings
    , type_talk :: TypeTalkSettings
    , cross_domain :: CrossDomainSettings
    , open_jtalk :: JTalkSettings
    } deriving (Generic)

instance FromJSON LinkSettings
instance FromJSON TypeTalkSettings
instance FromJSON CrossDomainSettings
instance FromJSON VoiceProperties
instance FromJSON JTalkSettings
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
