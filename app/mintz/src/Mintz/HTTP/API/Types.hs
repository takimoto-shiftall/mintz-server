{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Mintz.HTTP.API.Types where

import GHC.Generics
import GHC.TypeLits
import Data.String
import Data.Proxy
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Media
import Servant.API
import Servant.Server
import Data.Validation
import Ext.Servant

data Singular (key :: Symbol) a = Singular a

instance (KnownSymbol key, ToJSON a) => ToJSON (Singular key a) where
    toJSON (Singular v) = object [T.pack (symbolVal (Proxy :: Proxy key)) .= toJSON v]

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

instance Erroneous [ValidationError] where
    type ErroneousTypes [ValidationError] = '[JSON]

    buildError org e mt = buildError org (APIError 400 (map show e)) mt