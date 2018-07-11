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
import Mintz.Resource.Wechime
import Mintz.HTTP.API.Types

data PublishForm = PublishForm { message :: String
                               , kind :: String
                               , voice :: Maybe String
                               , channel :: String
                               , persons :: [Int]
                               , extra :: Maybe Object
                               }

data WechimeForm = WechimeForm { chimes :: [Int]
                               }

$(validatable [''PublishForm, ''WechimeForm])

type PublishAPI = "publish" :>
                ( Use LinkSettings
                    :> Use (M.Map String VoiceProperties)
                    :> ReqBody '[JSON] PublishForm' :> Post '[JSON] NoContent
             :<|> "wechime" :> ReqBody '[JSON] WechimeForm' :> Post '[JSON] NoContent
                )

publishAPI sc = publish' sc
           :<|> wechime' sc

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
            withContext @'[DB, REDIS, JTALK, CHATBOT, WECHIME] sc $ do
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
        ltrim s@(c:cs)
            | C.isSpace c = ltrim cs
            | C.isControl c = ltrim cs
            | otherwise = s

        trim :: String -> String
        trim s = reverse $ ltrim $ reverse $ ltrim s

wechime' :: SiteContext
         -> WechimeForm'
         -> Action NoContent
wechime' sc form = do
    case validate form of
        Nothing -> do
            throw $ errorFor err400 (errorsOf @WechimeForm form) sc
        Just f -> do
            withContext @'[WECHIME] sc $ do
                runWechime $ concat $ map toChime (chimes f)
            return NoContent
    where
        toChime :: Int -> [Chime]
        toChime 1 = [Chime1]
        toChime 2 = [Chime2]
        toChime _ = []