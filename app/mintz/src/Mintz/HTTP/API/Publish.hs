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
import qualified Data.ByteString.Lazy as BL
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
import Mintz.Settings hiding (wechime')
import Ext.Servant
import Data.Validation
import Mintz.Service.Publish
import Mintz.Resource.Wechime
import Mintz.HTTP.API.Types

-- ----------------------------------------------------------------
-- Data types.
-- ----------------------------------------------------------------

type PublishMessage = String :? '[MessageFormatter, Length String 1 1024]

data MessageFormatter

instance Verifier MessageFormatter where
    type VerifiableType MessageFormatter = String
    type VerifierSpec MessageFormatter = '[]

    verifierSpec _ = ("publish message", ANil)

    verify _ msg = Right $ trim msg
        where
            ltrim :: String -> String
            ltrim [] = []
            ltrim s@(c:cs)
                | C.isSpace c = ltrim cs
                | C.isControl c = ltrim cs
                | otherwise = s

            trim :: String -> String
            trim s = reverse $ ltrim $ reverse $ ltrim s

-- ----------------------------------------------------------------
-- Forms
-- ----------------------------------------------------------------

data PublishForm = PublishForm { message :: PublishMessage
                               , kind :: String :? '[Length String 0 64]
                               , voice :: Maybe (String :? '[Length String 0 32])
                               , channel :: String :? '[Length String 1 32]
                               , persons :: [Int]
                               , extra :: Maybe Object
                               }

data WechimeForm = WechimeForm { chimes :: [Int]
                               }

$(validatable [''PublishForm, ''WechimeForm])

-- ----------------------------------------------------------------
-- API
-- ----------------------------------------------------------------

type PublishAPI = "publish" :>
                ( Use LinkSettings
                    :> Use (M.Map String VoiceProperties)
                    :> ReqBody '[JSON] PublishForm' :> Post '[JSON] NoContent
             :<|> "wechime"
                    :> ReqBody '[JSON] WechimeForm' :> Post '[JSON] NoContent
             :<|> "sound"
                    :> Use LinkSettings
                    :> Capture "name" String
                    :> (
                        ReqBody '[OctetStream] BL.ByteString :> Post '[JSON] NoContent
                   :<|> Delete '[JSON] NoContent
                    )
                )

publishAPI sc = publish' sc
           :<|> wechime' sc
           :<|> soundAPI
    where
        soundAPI ls n = publishSound' sc ls n
                   :<|> deleteSound' sc ls n

-- ----------------------------------------------------------------
-- Handlers
-- ----------------------------------------------------------------

publish' :: SiteContext
         -> LinkSettings
         -> M.Map String VoiceProperties
         -> PublishForm'
         -> Action NoContent
publish' sc ls voices form = do
    f <- validateOr400 sc form
    let v = (.!) @PublishForm voice f >>= ((M.!?) voices) >>= return . path
    withContext @'[DB, REDIS, JTALK, CHATBOT, WECHIME] sc $ do
        let formatter = \p -> audio_url ls ++ p
        publishMessage (PublishEntry ((.!) @PublishForm message f)
                                     ((.!) @PublishForm kind f)
                                     v
                                     ((.!) @PublishForm channel f)
                                     ((.!) @PublishForm persons f)
                                     ((.!) @PublishForm extra f)
                       ) formatter (default_audio ls)
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
    f <- validateOr400 sc form
    withContext @'[WECHIME] sc $ do
        runWechime $ concat $ map toChime (chimes f)
    return NoContent
    where
        toChime :: Int -> [Chime]
        toChime 1 = [Chime1]
        toChime 2 = [Chime2]
        toChime _ = []

publishSound' :: SiteContext
              -> LinkSettings
              -> String
              -> BL.ByteString
              -> Action NoContent
publishSound' sc ls name sound = do
    let formatter = \p -> audio_url ls ++ p
    withContext @'[REDIS] sc $ do
        publishSound (sound_dir ls) name formatter sound
    return NoContent

deleteSound' :: SiteContext
             -> LinkSettings
             -> String
             -> Action NoContent
deleteSound' sc ls name = do
    removeSound (sound_dir ls) name
    return NoContent