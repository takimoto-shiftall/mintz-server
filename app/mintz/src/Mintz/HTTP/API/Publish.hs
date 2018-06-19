{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.HTTP.API.Publish where

import GHC.Generics
import Control.Exception
import Control.Monad.IO.Class
import Data.Maybe (maybe)
import Data.Proxy
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Servant.API
import Servant.Server
import Data.Aeson
import Control.Lens hiding ((:>))
import Data.Extensible hiding (Action)
import Web.FormUrlEncoded
import Data.Resource
import Ext.Servant.Action
import Ext.Servant.Context
import Mintz.Settings
import Ext.Servant.Validation
import Mintz.Service.Publish

data PublishForm = PublishForm { message :: String
                               , kind :: String
                               , voice :: Maybe String
                               , channel :: String
                               , persons :: Maybe Int
                               }

$(validatable [''PublishForm])

type PublishAPI = "publish" :> ReqBody '[JSON] PublishForm' :> Post '[JSON] NoContent

publishAPI sc = publish' sc

publish' :: SiteContext
         -> PublishForm'
         -> Action NoContent
publish' sc form = do
    case validate form of
        Nothing -> do
            print $ errors (Proxy :: Proxy PublishForm) form
            return ()
        Just f -> do
            withContext @'[DB, REDIS, JTALK, CHATBOT] sc $ do
                publishMessage (message f) (kind f) (voice f) (channel f) ((:[]) <$> persons f)
            return ()
    return NoContent