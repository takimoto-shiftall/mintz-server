{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Mintz.Hspec.Settings where

import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Mintz.Settings (Database(..))

db = Database (PostgreSQL "postgresql://postgres:postgres@172.16.147.59:15433/mintz" 10)

type DB = DBContext Database