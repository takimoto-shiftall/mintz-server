{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Mintz.Resource.TypeTalk.Auth where

import GHC.Generics
import Control.Monad
import qualified Data.ByteString as BS
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple

data Credential = Credential {
      access_token :: String
    , token_type :: String
    , expires_in :: Int
    , refresh_token :: String
    } deriving (Show, Generic)

instance FromJSON Credential

getCredential :: BS.ByteString -> BS.ByteString -> IO Credential
getCredential cid secret = do
    req <- setRequestBodyURLEncoded
            <$> return [ ("client_id", cid)
                       , ("client_secret", secret)
                       , ("grant_type", "client_credentials")
                       , ("scope", "topic.read")
                       ]
            <*> parseRequest "https://typetalk.com/oauth2/access_token" >>= \r -> return (r { method = "POST" })
    getResponseBody <$> httpJSON req
