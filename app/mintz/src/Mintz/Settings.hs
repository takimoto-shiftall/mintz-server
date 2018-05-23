{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Mintz.Settings where

import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Ext.Servant.Context
import Mintz.Resource.Redis

newtype Database = Database PostgreSQL deriving (DBSettings)

db = Database (PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/mintz" 10)

type DB = DBContext Database
type PubSub = RedisPubSubContext

type SiteKeys = '[]

type SiteContext = RequestContext SiteKeys (Refs '[DB, PubSub, Logger])

publicPath = "/public"