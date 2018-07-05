{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import GHC.Generics
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Aeson
import Control.Lens
import Data.Extensible
import Text.Parsec
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Mintz.Resource.TypeTalk.Auth
import Mintz.Resource.TypeTalk.Subscribe

data PublishForm = PublishForm { message :: String
                               , kind :: String
                               , voice :: Maybe String
                               , channel :: String
                               , persons :: [Int]
                               , extra :: Maybe Object
                               } deriving (Generic)

instance ToJSON PublishForm

data SubscriptionSettings = SubscriptionSettings { client_id :: String
                                                 , client_secret :: String
                                                 , topics :: [Int]
                                                 } deriving (Generic)

instance FromJSON SubscriptionSettings

main :: IO ()
main = do
    settings <- decodeFileThrow "config/daemon-develop.yml" :: IO SubscriptionSettings

    credential <- getCredential (client_id settings)
                                (client_secret settings)

    (status, closer) <- subscribe credential $ \msg -> do
        case eitherDecode msg :: Either String SubscriptionMessage of
            Left e -> print e
            Right m -> case m of
                PostMessage t p -> do
                    let msg = view #message p
                    if view #topicId p `elem` topics settings
                        then
                            case runParser parseMessage False "" msg of
                                Left e -> print e
                                Right (toPublish, s) -> do
                                    --putStrLn $ (show toPublish) ++ ": " ++ s
                                    if toPublish then publishMessage s
                                                 else return ()
                        else
                            return ()
                UnknownMessage s -> print s

    forkIO $ forever $ do
        command <- getLine
        case command of
            "q" -> closer
            _ -> return ()

    readMVar status >>= print

publishMessage :: String
               -> IO ()
publishMessage msg = do
    let body = encode $ PublishForm { message = msg
                                    , kind = "test"
                                    , voice = Nothing
                                    , channel = "mintz"
                                    , persons = []
                                    , extra = Nothing
                                    }
    req <- parseRequest "http://mintz.local:8001/api/publish"
            >>= \r -> return (r { method = "POST" })
            >>= return . setRequestBody (RequestBodyLBS body)
            >>= return . setRequestHeader "Content-type" ["application/json"]

    res <- httpNoBody req

    print $ "Response from mintz-server: " ++ show (getResponseStatus res)

parseMessage :: Parsec String Bool (Bool, String)
parseMessage = do
    parts <- many (try urlike <|> try emoji <|> normal)
    (, concat parts) <$> getState
    where
        normal :: Parsec String u String
        normal = anyChar >>= return . (:[])

        urlike :: Parsec String u String
        urlike = do
            sequence [string "http", option "" (string "s"), string "://"]
            manyTill anyChar (try $ lookAhead (space <|> oneOf "\\'|`^\"<>(){}[]"))
            return ""

        emoji :: Parsec String Bool String
        emoji = do
            char ':'
            t <- many1 (try alphaNum <|> char '_')
            char ':'
            if t == "speech_balloon" then setState True else return () 
            return ""
