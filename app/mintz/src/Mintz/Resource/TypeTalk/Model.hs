{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Mintz.Resource.TypeTalk.Model where

import Data.Time.Clock
import Data.Extensible

type Topic = Record '[
    "id" >: Int
  , "name" >: String
  , "description" >: String
  , "suggestion" >: String
  , "isDirectMessage" >: Bool
  , "lastPostedAt" >: UTCTime
  , "createdAt" >: UTCTime
  , "updatedAt" >: UTCTime
  ]

type Unread = Record '[
    "topicId" >: Int
  , "postId" >: Int
  , "count" >: Int
  , "isOverCountLimit" >: Bool
  ]

type TopicItem = Record '[
    "topic" >: Topic
  , "favorite" >: Bool
  , "unread" >: Unread
  ]

type Account = Record '[
    "id" >: Int
  , "name" >: String
  , "fullName" >: String
  , "suggestion" >: String
  , "imageUrl" >: String
  , "isBot" >: Bool
  , "createdAt" >: UTCTime
  , "updatedAt" >: UTCTime
  ]

type Like = Record '[
    "id" >: Int
  , "postId" >: Int
  , "topicId" >: Int
  , "comment" >: Maybe String
  , "account" >: Account
  , "createdAt" >: UTCTime
  ]

type Talk = Record '[
    "id" >: Int
  , "topicId" >: Int
  , "name" >: String
  , "suggestion" >: String
  , "createdAt" >: UTCTime
  , "updatedAt" >: UTCTime
  ]

type TopicPost = Record '[
    "id" >: Int
  , "topicId" >: Int
  , "replyTo" >: Maybe Int
  , "message" >: String
  , "account" >: Account
  , "likes" >: [Like]
  , "talks" >: [Talk]
  , "createdAt" :> UTCTime
  , "updatedAt" :> UTCTime
  ]

type Sub'Topic = Record '[
    "id" >: Int
  , "name" >: String
  , "isDirectMessage" >: Bool
  , "lastPostedAt" >: UTCTime
  , "createdAt" >: UTCTime
  , "updatedAt" >: UTCTime
  ]

type Sub'Post = Record '[
    "id" >: Int
  , "topicId" >: Int
  , "replyTo" >: Maybe Int
  , "message" >: String
  , "createdAt" >: UTCTime
  , "updatedAt" >: UTCTime
  ]