{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.HTTP.API.Publish where

import GHC.Generics
import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Maybe (maybe)
import qualified Data.Char as C
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
import Mintz.HTTP.API.Types

data PublishForm = PublishForm { message :: String
                               , kind :: String
                               , voice :: Maybe String
                               , channel :: String
                               , persons :: [Int]
                               , extra :: Maybe Object
                               }

$(validatable [''PublishForm])

type PublishAPI = "publish" :> Use LinkSettings :> Use (M.Map String VoiceProperties)
                :> ReqBody '[JSON] PublishForm' :> Post '[JSON] NoContent

publishAPI sc = publish' sc

publish' :: SiteContext
         -> LinkSettings
         -> M.Map String VoiceProperties
         -> PublishForm'
         -> Action NoContent
publish' sc ls voices form = do
    case validate form of
        Nothing -> do
            throw $ errorFor err400 (errorsOf @PublishForm form) sc
        Just f -> do
            let v = voice (f :: PublishForm) >>= ((M.!?) voices) >>= return . path
            withContext @'[DB, REDIS, JTALK, CHATBOT] sc $ do
                let formatter = \p -> audio_url ls ++ p
                publishMessage (PublishEntry (trim $ message (f :: PublishForm))
                                             (kind (f :: PublishForm))
                                             v
                                             (channel (f :: PublishForm))
                                             (persons (f :: PublishForm))
                                             (extra (f :: PublishForm)))
                               formatter
                               (default_audio ls)
            return NoContent
    where
        ltrim :: String -> String
        ltrim [] = []
        ltrim (c:cs)
            | C.isSpace c = trim cs
            | C.isControl c = trim cs
            | otherwise = c : trim cs

        trim :: String -> String
        trim s = reverse $ ltrim $ reverse $ ltrim s