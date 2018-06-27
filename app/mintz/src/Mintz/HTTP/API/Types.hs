{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mintz.HTTP.API.Types where

import GHC.Generics
import Data.String
import Data.Aeson
import Network.HTTP.Media
import Servant.API
import Servant.Server
import Ext.Servant.Context

type ErrorCode = Int

data APIError = APIError { code :: ErrorCode
                         , messages :: [String]
                         } deriving (Show, Generic)

instance ToJSON APIError

instance Erroneous APIError where
    type ErroneousTypes APIError = '[JSON]

    buildError org e mt
        | mt `matches` ("application" // "json") =
            org { errBody = encode e }
        | otherwise =
            org { errBody = fromString ("Code = " ++ show (code e) ++ ", Message = " ++ show (messages e)) }