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
import Data.Aeson
import Data.Yaml hiding (encode)
import Control.Lens
import Data.Extensible
import Text.Parsec
import Network.HTTP.Conduit
import Network.HTTP.Simple
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

data SubscriptionSettings = SubscriptionSettings { publish_url :: String
                                                 , client_id :: String
                                                 , client_secret :: String
                                                 , topics :: [Int]
                                                 , speech_key :: String
                                                 , voice_keys :: M.Map String String
                                                 , maximum_length :: Maybe Int
                                                 } deriving (Generic)

instance FromJSON SubscriptionSettings

main :: IO ()
main = do
    settings <- decodeFileEither "config/daemon-develop.yml" >>= either throw return

    credential <- getCredential (fromString $ client_id settings)
                                (fromString $ client_secret settings)

    (status, closer) <- subscribe credential $ \msg -> do
        case eitherDecode msg :: Either String SubscriptionMessage of
            Left e -> print e
            Right m -> case isPublishable settings m of
                        Just (msg, v) -> do
                            publishMessage (publish_url settings) msg v
                        _ -> return ()

    forkIO $ forever $ do
        command <- getLine
        case command of
            "q" -> closer
            _ -> return ()

    readMVar status >>= print

publishMessage :: String
               -> String
               -> Maybe String
               -> IO ()
publishMessage url msg v = do
    print $ "Publish message: " ++ msg ++ " (voice: " ++ maybe "" id v ++ ")"
    let body = encode $ PublishForm { message = msg
                                    , kind = "test"
                                    , voice = v
                                    , channel = "mintz"
                                    , persons = []
                                    , extra = Nothing
                                    }
    req <- parseRequest (fromString url)
            >>= \r -> return (r { method = "POST" })
            >>= return . setRequestBody (RequestBodyLBS body)
            >>= return . setRequestHeader "Content-type" ["application/json"]

    res <- httpNoBody req

    print $ "Response from mintz-server: " ++ show (getResponseStatus res)

isPublishable :: SubscriptionSettings
              -> SubscriptionMessage
              -> Maybe (String, Maybe String)
isPublishable settings (PostMessage _ p) = do
    msg <- if view #topicId p `elem` topics settings then Just (view #message p) else Nothing
    case runParser parseMessage [] "" msg of
            Left e -> Nothing
            Right (msg', keys) -> if not (speech_key settings `elem` keys) then Nothing else do 
                m <- let maxlen = maybe defaultMaximumLength id (maximum_length settings)
                     in if length msg' <= maxlen then return msg' else Nothing
                return (m, getFirst $ mconcat $ map (First . (voice_keys settings M.!?)) keys)
isPublishable _ _ = Nothing

parseMessage :: Parsec String [String] (String, [String])
parseMessage = do
    parts <- many (try urlike <|> try emoji <|> normal)
    (concat parts,) <$> getState
    where
        normal :: Parsec String u String
        normal = anyChar >>= return . (:[])

        urlike :: Parsec String u String
        urlike = do
            sequence [string "http", option "" (string "s"), string "://"]
            manyTill anyChar (try $ lookAhead (space <|> oneOf "\\'|`^\"<>(){}[]"))
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
