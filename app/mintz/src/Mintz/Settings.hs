{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Mintz.Settings where

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

openJTalk = OpenJTalk { dictDir = "/usr/local/Cellar/open-jtalk/1.10_1/dic"
                      , voiceDir = "/usr/local/Cellar/open-jtalk/1.10_1/voice"
                      , outDir = "./public/voice"
                      }

-- FIXME
typeTalk = TypeTalkBot "" 0

type DB = DBContext Database
type REDIS = RedisPubSubContext
type JTALK = OpenJTalkContext
type CHATBOT = TypeTalkBotContext

type SiteKeys = '[]

type SiteContext = RequestContext SiteKeys (Refs '[DB, REDIS, JTALK, CHATBOT, Logger])

publicPath = "/public"