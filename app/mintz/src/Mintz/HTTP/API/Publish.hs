{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.HTTP.API.Publish where

import GHC.Generics
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Map as M
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
import Ext.Servant.Context
import Ext.Servant.Validation
import Mintz.Service.Publish

data PublishForm = PublishForm { message :: String
                               , kind :: String
                               , voice :: Maybe String
                               , channel :: String
                               , persons :: [Int]
                               }

$(validatable [''PublishForm])

type PublishAPI = "publish" :> Use PublishSettings :> Use (M.Map String VoiceProperties)
                :> ReqBody '[JSON] PublishForm' :> Post '[JSON] NoContent

publishAPI sc = publish' sc

publish' :: SiteContext
         -> PublishSettings
         -> M.Map String VoiceProperties
         -> PublishForm'
         -> Action NoContent
publish' sc ps voices form = do
    case validate form of
        Nothing -> do
            print $ errors (Proxy :: Proxy PublishForm) form
            return ()
        Just f -> do
            withContext @'[DB, REDIS, JTALK, CHATBOT] sc $ do
                let formatter = \p -> audio_url ps ++ p
                publishMessage (message f) (kind f) (voice f) (channel f) (persons f) formatter
            return ()
    return NoContent