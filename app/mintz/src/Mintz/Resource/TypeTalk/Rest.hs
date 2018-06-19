{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Mintz.Resource.TypeTalk.Rest where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.IORef
import Data.ByteString
import Data.String
import Data.CaseInsensitive
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Data.Resource

typeTalkTag = "Network.TypeTalk"

typeTalkApiUrl :: String
               -> String
typeTalkApiUrl path = "https://typetalk.com/api" ++ path

authenticationHeader :: ByteString
authenticationHeader = "X-Typetalk-Token"

data TypeTalkBot = TypeTalkBot String Int
data TypeTalkBotContext = TypeTalkBotContext TypeTalkBot

instance Resource TypeTalkBot where
    type ContextType TypeTalkBot = TypeTalkBotContext

    newContext r = do
        bot <- liftIO $ readIORef r
        liftIO $ newIORef (TypeTalkBotContext bot)

instance ResourceContext TypeTalkBotContext where
    type ResourceType TypeTalkBotContext = TypeTalkBot

    closeContext c _ = return c
    execContext cr f = f

makeRequest :: (MonadThrow m)
            => TypeTalkBot
            -> String
            -> m Request
makeRequest (TypeTalkBot token _) path =
    setRequestHeader (mk authenticationHeader) [fromString token]
        <$> parseRequest (typeTalkApiUrl path)

get :: forall a m. (MonadIO m, MonadThrow m, FromJSON a)
    => TypeTalkBotContext
    -> String
    -> Query
    -> m a
get (TypeTalkBotContext bot) path q = do
    r <- setRequestQueryString q <$> makeRequest bot path
    getResponseBody <$> httpJSON (r { method = "GET" })

post :: forall a m. (MonadIO m, MonadThrow m, ToJSON a)
     => TypeTalkBotContext
     -> String
     -> a
     -> m (Response ())
post (TypeTalkBotContext bot) path body = do
    r <- setRequestBody (RequestBodyLBS $ encode body) <$> makeRequest bot path
    httpNoBody ((setRequestHeader "Content-Type" ["application/json"] r) { method = "POST" })
