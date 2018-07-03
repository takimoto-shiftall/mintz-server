{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Mintz.Resource.TypeTalk.API where

import Data.IORef
import Data.String (fromString)
import Data.Maybe (catMaybes, maybe)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Time.Clock
import Data.Extensible
import Control.Lens hiding ((.=))
import Data.Aeson
import Network.HTTP.Simple
import Data.Resource
import Mintz.Resource.TypeTalk.Rest
import Mintz.Resource.TypeTalk.Model

type TopicMessages = Record '[
    "topic" >: Topic
  , "posts" >: [TopicPost]
  ]

maximumMessageCount = 200

topicMessages :: (With '[TypeTalkBotContext])
              => Maybe Int
              -> Maybe Int
              -> Bool
              -> IO TopicMessages
topicMessages count from forward = do
    bot@(TypeTalkBotContext (TypeTalkBot _ topicId)) <- readIORef $ contextOf @TypeTalkBotContext ?cxt

    $(logQD' typeTalkTag) ?cxt $
        "Start trying to get messages of topic#" ++ show topicId
            ++ " (from: #" ++ show (maybe 0 id from)
            ++ ", count: " ++ maybe "unspecified" show count ++ ")"

    let conv k = (k, ) . Just . fromString . show
    let q = catMaybes [ conv "count" <$> count
                      , conv "from" <$> from
                      , Just ("direction", Just $ if forward then "forward" else "backward")]
    messages <- get bot ("/v1/topics/" ++ show topicId) q

    $(logQD' typeTalkTag) ?cxt $
        "    " ++ show (length (view #posts messages)) ++ " messages are obtained"

    return messages

allTopicMessages :: (With '[TypeTalkBotContext])
                 => Maybe Int
                 -> IO [TopicPost]
allTopicMessages latest = get (maybe 0 id latest)
    where
        get :: Int -> IO [TopicPost]
        get mid = do 
            messages <- topicMessages (Just maximumMessageCount) (Just mid) True
            let posts = view #posts messages
            case reverse posts of
                [] -> return []
                p:_ -> (posts ++) <$> allTopicMessages (Just $ view #id p)

postMessage :: (With '[TypeTalkBotContext])
            => String
            -> [String]
            -> IO Int
postMessage message accounts = do 
    bot@(TypeTalkBotContext (TypeTalkBot _ topicId)) <- readIORef $ contextOf @TypeTalkBotContext ?cxt

    let message' = L.intercalate " " $ (map ('@':) accounts) ++ [message]

    res <- post bot ("/v1/topics/" ++ show topicId)
                    (object ["message" .= message'])

    $(logQD' typeTalkTag) ?cxt $ "Reponse for message post: " ++ show res

    return $ getResponseStatusCode res