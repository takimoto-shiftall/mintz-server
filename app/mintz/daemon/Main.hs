{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import GHC.Generics
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Safe hiding (try)
import Data.Maybe (maybe)
import Data.Either (either)
import Data.String
import Data.Monoid
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Yaml hiding (encode)
import Control.Lens
import Data.Extensible
import Text.Parsec
import Network.HTTP.Conduit
import Network.HTTP.Simple
import qualified Network.HTTP.Client as H
import Mintz.Resource.TypeTalk.Auth
import Mintz.Resource.TypeTalk.Subscribe

defaultMaximumLength = 40 :: Int

data PublishForm = PublishForm { message :: String
                               , kind :: String
                               , voice :: Maybe String
                               , channel :: String
                               , persons :: [Int]
                               , extra :: Maybe Object
                               } deriving (Generic)

instance ToJSON PublishForm

data WechimeForm = WechimeForm { chimes :: [Int]
                               } deriving (Generic)

instance ToJSON WechimeForm

data SubscriptionSettings = SubscriptionSettings { publish_url :: String
                                                 , wechime_url :: String
                                                 , client_id :: String
                                                 , client_secret :: String
                                                 , topics :: [Int]
                                                 , speech_key :: String
                                                 , wechime_key :: String
                                                 , voice_keys :: M.Map String String
                                                 , maximum_length :: Maybe Int
                                                 } deriving (Generic)

instance FromJSON SubscriptionSettings

main :: IO ()
main = do
    print "mintz-daemon launches..."

    settings <- decodeFileEither "config/daemon-develop.yml" >>= either throw return

    credential <- getCredential (fromString $ client_id settings)
                                (fromString $ client_secret settings)

    (status, closer) <- subscribe credential $ \msg -> do
        case eitherDecode msg :: Either String SubscriptionMessage of
            Left e -> print e
            Right m -> case makeRequest settings m of
                        Just (url, body) -> do
                            publishMessage url body
                        _ -> return ()

    forkIO $ forever $ do
        command <- getLine
        case command of
            "q" -> closer
            _ -> return ()

    readMVar status >>= print

    print "shutdown mintz-daemon"

publishMessage :: String
               -> BL.ByteString
               -> IO ()
publishMessage url body = do
    req <- parseRequest (fromString url)
            >>= \r -> return (r { method = "POST" })
            >>= return . setRequestBody (RequestBodyLBS body)
            >>= return . setRequestHeader "Content-type" ["application/json"]

    res <- httpNoBody $ req { responseTimeout = H.responseTimeoutNone }

    print req
    print body

    print $ "Response from mintz-server: " ++ show (getResponseStatus res)

makeRequest :: SubscriptionSettings
            -> SubscriptionMessage
            -> Maybe (String, BL.ByteString)
makeRequest settings (PostMessage _ p)
    | view #topicId p `elem` topics settings = 
        case runParser parseMessage [] "" (view #message p) of
            Left e -> Nothing
            Right (msg, keys) -> getFirst $ mconcat $ map First [ encodeBody <$> toPublishForm settings msg keys
                                                                , encodeBody <$> toWechimeForm settings msg keys
                                                                ]
    | otherwise = Nothing
    where
        encodeBody :: (ToJSON a) => (String, a) -> (String, BL.ByteString)
        encodeBody (url, v) = (url, encode v)
makeRequest _ _ = Nothing

toPublishForm :: SubscriptionSettings
              -> String
              -> [String]
              -> Maybe (String, PublishForm)
toPublishForm settings msg keys
    | speech_key settings `elem` keys = (publish_url settings, ) <$>
        if length msg <= maxlen
            then Just PublishForm { message = msg
                                  , kind = "speech"
                                  , voice = voice'
                                  , channel = "mintz"
                                  , persons = []
                                  , extra = extra'
                                  }
            else Nothing
    | otherwise = Nothing
    where
        maxlen = maybe defaultMaximumLength id (maximum_length settings)
        voice' = getFirst $ mconcat $ map (First . (voice_keys settings M.!?)) keys
        extra' = if wechime_key settings `elem` keys
                    then Just $ HM.fromList [("wechime", toJSONList [1 :: Int])]
                    else Nothing

toWechimeForm :: SubscriptionSettings
              -> String
              -> [String]
              -> Maybe (String, WechimeForm)
toWechimeForm settings msg keys
    | wechime_key settings `elem` keys = Just (wechime_url settings, WechimeForm [1])
    | otherwise = Nothing

--isPublishable :: SubscriptionSettings
--              -> SubscriptionMessage
--              -> Maybe (String, Maybe String, Bool)
--isPublishable settings (PostMessage _ p) = do
--    msg <- if view #topicId p `elem` topics settings then Just (view #message p) else Nothing
--    case runParser parseMessage [] "" msg of
--            Left e -> Nothing
--            Right (msg', keys) -> if not (speech_key settings `elem` keys) then Nothing else do 
--                m <- let maxlen = maybe defaultMaximumLength id (maximum_length settings)
--                     in if length msg' <= maxlen then return msg' else Nothing
--                return ( m
--                       , getFirst $ mconcat $ map (First . (voice_keys settings M.!?)) keys
--                       , wechime_key settings `elem` keys
--                       )
--isPublishable _ _ = Nothing

parseMessage :: Parsec String [String] (String, [String])
parseMessage = do
    parts <- many (try urlike <|> try emoji <|> try mention <|> normal)
    (concat parts,) <$> getState
    where
        normal :: Parsec String u String
        normal = anyChar >>= return . (:[])

        urlike :: Parsec String u String
        urlike = do
            sequence [string "http", option "" (string "s"), string "://"]
            manyTill anyChar (try $ lookAhead (space <|> oneOf "\\'|`^\"<>(){}[]"))
            return ""

        mention :: Parsec String [String] String
        mention = do
            spaces
            char '@'
            to <- manyTill (try alphaNum <|> try (char '_') <|> try (char '+') <|> char '-') (try space)
            spaces
            return ""

        emoji :: Parsec String [String] String
        emoji = do
            spaces
            char ':'
            t <- many1 (try alphaNum <|> char '_')
            char ':'
            spaces
            modifyState (t :)
            return ""
