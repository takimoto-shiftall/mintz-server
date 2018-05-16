module Mintz.Resource.Redis where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Exception.Safe
import Data.IORef
import Database.Redis
import Data.Resource

data RedisPubSub = RedisPubSub ConnectInfo

data RedisPubSubContext = RedisPubSubContext Connection

instance Resource RedisPubSub where
    type ContextType RedisPubSub = RedisPubSubContext

    newContext ref = liftIO $ do
        RedisPubSub c <- readIORef ref
        newIORef =<< RedisPubSubContext <$> connect c

instance ResourceContext RedisPubSubContext where
    type ResourceType RedisPubSubContext = RedisPubSub

    closeContext cxt status = return cxt

    -- RunInBase m IO == m a -> IO (StM m a)
    -- control :: (RunInBase m IO -> IO (StM m a)) -> m a
    --         == ((m a -> IO (StM m a)) -> IO (StM m a)) -> m a
    execContext ref action = do
        RedisPubSubContext conn <- liftIO $ readIORef ref
        control $ \runInBase -> do
            runInBase action `finally` (runRedis conn quit)

