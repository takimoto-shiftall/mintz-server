{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Mintz.Settings where

import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Mintz.Resource.Redis

newtype Database = Database PostgreSQL deriving (DBSettings)

db = Database (PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/mintz" 10)

type DB = DBContext Database
type PubSub = RedisPubSubContext
