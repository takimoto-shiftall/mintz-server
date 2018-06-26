{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.HTTP.API.Voice where

import GHC.Generics
import Control.Exception
import Data.Maybe (maybe)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Servant.API
import Servant.Server
import Data.Aeson
import Ext.Servant.Action
import Ext.Servant.Context
import Ext.Servant.Validation
import Mintz.Settings

type VoiceAPI = "voice" :> Use (M.Map String VoiceProperties) :>
               ( Get '[JSON] [VoiceItem]
               )

data VoiceItem = VoiceItem { key :: String
                           , description :: String
                           } deriving (Generic)

instance ToJSON VoiceItem

voiceAPI sc = index' sc

index' :: SiteContext
       -> M.Map String VoiceProperties
       -> Action [VoiceItem]
index' sc voices = do
    return $ map toItem (M.toList voices)
    where
        toItem :: (String, VoiceProperties) -> VoiceItem
        toItem (k, vp) = VoiceItem k (Mintz.Settings.description vp)