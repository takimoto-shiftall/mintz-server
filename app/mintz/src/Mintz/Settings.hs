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

newtype Database = Database PostgreSQL deriving (DBSettings)

db = Database (PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/mintz" 10)

openJTalk = OpenJTalk { dictDir = "/usr/local/Cellar/open-jtalk/1.10_1/dic"
                      , voiceFile = "/usr/local/Cellar/open-jtalk/1.10_1/voice/mei/mei_happy.htsvoice"
                      , outDir = "./public/voice"
                      }

type DB = DBContext Database
type REDIS = RedisPubSubContext
type JTALK = OpenJTalkContext

type SiteKeys = '[]

type SiteContext = RequestContext SiteKeys (Refs '[DB, REDIS, JTALK, Logger])

publicPath = "/public"