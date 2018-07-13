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
import Data.Maybe (maybe, catMaybes)
import Data.Either (either)
import Data.String
import Data.Monoid
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Yaml hiding (encode)
import Control.Monad.Logger
import System.Log.FastLogger
import Control.Lens
import Data.Extensible
import Text.Parsec
import Network.HTTP.Conduit
import Network.HTTP.Simple
import qualified Network.HTTP.Client as H
import Data.Resource
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
                                                 , silent_key :: String
                                                 , wechime_key :: String
                                                 , voice_keys :: M.Map String String
                                                 , maximum_length :: Maybe Int
                                                 } deriving (Generic)

instance FromJSON SubscriptionSettings

main :: IO ()
main = do
    print "mintz-daemon has launched..."

    settings <- decodeFileEither "config/daemon-develop.yml" >>= either throw return

    lr <- newLoggingResource [(anyTag, LevelDebug, LogStdout defaultBufSize, Nothing)] >>= newIORef
    tr <- newIORef $ TypeTalkSubscription (client_id settings) (client_secret settings)

    let resources = tr `RCons` lr `RCons` RNil

    ((status, closer), _) <- withContext @'[SubscriptionContext] resources $ do
        logD $ "Connection established. Subscription has started..."

        let pong = logD "Pong arrived"

        (`forkSubscription` pong) $ \msg -> do
            case eitherDecode msg :: Either String SubscriptionMessage of
                Left e -> logD $ show e
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

publishMessage :: (With '[SubscriptionContext])
               => String
               -> BL.ByteString
               -> IO ()
publishMessage url body = do
    req <- parseRequest (fromString url)
            >>= \r -> return (r { method = "POST" })
            >>= return . setRequestBody (RequestBodyLBS body)
            >>= return . setRequestHeader "Content-type" ["application/json"]

    logD $ "Send request to mintz-server:"
    logD $ show req

    res <- httpNoBody $ req { responseTimeout = H.responseTimeoutNone }

    logD $ "Response was received from mintz-server:"
    logD $ show (getResponseStatus res)

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
        extra' = Just $ HM.fromList
                      $ catMaybes [ wechime_key settings `L.elemIndex` keys >> Just ("wechime", toJSONList [1 :: Int])
                                  , silent_key settings `L.elemIndex` keys >> Just ("silent", Bool True)
                                  ]

toWechimeForm :: SubscriptionSettings
              -> String
              -> [String]
              -> Maybe (String, WechimeForm)
toWechimeForm settings msg keys
    | wechime_key settings `elem` keys = Just (wechime_url settings, WechimeForm [1])
    | otherwise = Nothing

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
