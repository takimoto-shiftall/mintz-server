{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Mintz.Settings where

import GHC.Generics
import qualified Data.Map as M
import Data.Aeson
import Network.Wai
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Ext.Servant
import Data.Config
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk
import Mintz.Resource.TypeTalk
import Mintz.Resource.Wechime

{- version 0 -}

newtype Database = Database PostgreSQL deriving (DBSettings)

db = Database (PostgreSQL "postgresql://postgres:postgres@172.16.147.59:15432/mintz" 10)
--db = Database (PostgreSQL "postgresql://postgres:postgres@172.27.146.49:15432/mintz" 10)

type DB = DBContext Database
type REDIS = RedisPubSubContext
type JTALK = OpenJTalkContext
type CHATBOT = TypeTalkBotContext
type WECHIME = WechimeContext

-- | Declare configuration type by YAML.
$(yamlConfiguration "config/default.yml" "Settings" [])

type LinkSettings = Settings'link
type VoiceProperties = Settings'open_jtalk'voiceProperty

-- | Types of @Resource@s used in the application.
type AppResources = '[ DBResource Database
                     , RedisPubSub
                     , OpenJTalk
                     , TypeTalkBot
                     , Wechime
                     , LoggingResource
                     ]

-- | Types of @Vault@ keys in @RequestContext@.
type AppKeys = '[]

-- | Types of @Context@s for the application.
type AppContexts = '[ LinkSettings
                    , CrossDomainOrigin
                    , M.Map String VoiceProperties
                    ]

type SiteContext = RequestContext AppKeys (Refs '[DB, REDIS, JTALK, CHATBOT, WECHIME, Logger])

(@@) :: (Contexts (Refs '[DB, REDIS, JTALK, CHATBOT, WECHIME, Logger]) -> a)
     -> SiteContext
     -> a
(@@) f (RequestContext _ cs _) = f cs

publicPath = "/public"