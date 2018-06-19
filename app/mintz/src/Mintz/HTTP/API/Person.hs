{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.HTTP.API.Person where

import GHC.Generics
import Control.Exception
import Data.Maybe (maybe)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Servant.API
import Servant.Server
import Data.Aeson
import Control.Lens hiding ((:>))
import Data.Extensible hiding (Action)
import Data.Resource
import Database.ORM
import Ext.Servant.Action
import Ext.Servant.Context
import Ext.Servant.Validation
import Mintz.Settings
import Mintz.Model.Models
import Mintz.Model.Types
import Mintz.Service.Person

defaultListLength = 100
defaultLang = "ja"

type PersonAPI = "person" :> 
               ( QueryParam "limit" Int
                          :> QueryParam "offset" Int
                          :> QueryParam "lang" String
                          :> Get '[JSON] [PersonItem]
               )

data PersonItem = PersonItem { id :: Integer
                             , name :: String
                             , description :: String
                             } deriving (Generic)

instance ToJSON PersonItem

personAPI sc = index' sc

index' :: SiteContext
       -> Maybe Int
       -> Maybe Int
       -> Maybe String
       -> Action [PersonItem]
index' sc limit offset lang = do
    (persons, _) <- withContext @'[DB] sc $ do
        listPersons (maybe defaultListLength Prelude.id limit) (maybe 0 Prelude.id offset)
    return $ map (model2Item $ maybe defaultLang Prelude.id lang) persons

model2Item :: String
           -> Person
           -> PersonItem
model2Item lang m = let r = getRecord m
                    in PersonItem { id = view #id r
                                  , name = lbl (view #last_name r) ++ " " ++ lbl (view #first_name r)
                                  , description = lng (view #description r)
                                  }
    where
        lbl = labelOf lang
        lng = langOf lang