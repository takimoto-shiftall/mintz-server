{-# LANGUAGE OverloadedStrings #-}

module Mintz.Service.Publish where

import Data.Proxy
import Data.IORef
import Data.String
import qualified Data.ByteString as B
import Data.Resource
import Database.Redis
import Mintz.Settings (REDIS)
import Mintz.Resource.Redis

publishMessage :: (With '[REDIS])
               => String
               -> IO ()
publishMessage message = do
    (RedisPubSubContext conn) <- readIORef $ contextOf @REDIS ?cxt
    runRedis conn $ publish "mintz" (fromString message)
    return ()