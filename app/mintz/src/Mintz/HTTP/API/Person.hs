{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.HTTP.API.Person where

import GHC.Generics
import Control.Monad.Except
import Control.Exception
import Data.Maybe (maybe)
import Text.Printf
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
import Ext.Servant.Combinators
import Mintz.Settings
import Mintz.Model.Models
import Mintz.Model.Types
import Mintz.Service.Person
import Mintz.HTTP.API.Types

defaultListLength = 100
defaultLang = "ja"

type PersonAPI = "person" :> Use LinkSettings :>
               ( QueryParam "limit" Int
                          :> QueryParam "offset" Int
                          :> QueryParam "lang" String
                          :> Get '[JSON] Persons
               )

data PersonItem = PersonItem { id :: Integer
                             , name :: String
                             , reading :: String
                             , nickname :: String
                             , icon :: String
                             , description :: String
                             } deriving (Generic)

data Persons = Persons { persons :: [PersonItem]
                       } deriving (Generic)

instance ToJSON PersonItem
instance ToJSON Persons

personAPI sc = index' sc

index' :: SiteContext
       -> LinkSettings
       -> Maybe Int
       -> Maybe Int
       -> Maybe String
       -> Action Persons
index' sc link limit offset lang = do
    (persons, _) <- withContext @'[DB] sc $ do
        listPersons (maybe defaultListLength Prelude.id limit) (maybe 0 Prelude.id offset)
    return $ Persons $ map (model2Item (icon_url link) $ maybe defaultLang Prelude.id lang) persons

model2Item :: String
           -> String
           -> Person
           -> PersonItem
model2Item url lang m = let r = getRecord m
                            fn = view #first_name r
                            ln = view #last_name r
                            iconUrl = maybe "" ((url ++) . (++ ".png")) (typeTalkName m)
                        in PersonItem { id = view #id r
                                      , name = lbl fn ++ " " ++ lbl ln
                                      , reading = rdg fn ++ " " ++ rdg ln
                                      , nickname = lbl (view #nickname r)
                                      , icon = iconUrl
                                      , description = lng (view #description r)
                                      }
    where
        lbl = labelOf lang
        rdg = readingOf lang
        lng = langOf lang

data ReadableString = ReadableString { text :: String
                                     , reading :: String
                                     } deriving (Show, Generic)

data MultiReadable = MultiReadable { en :: ReadableString
                                   , ja :: ReadableString
                                   } deriving (Show, Generic)

data MultiLang = MultiLang { en :: String
                           , ja :: String
                           } deriving (Show, Generic)

data PersonForm = PersonForm { first_name :: MultiReadable
                             , middle_name ::MultiReadable
                             , last_name :: MultiReadable
                             , nickname :: MultiReadable
                             , description :: MultiLang
                             , typetalk_name :: String
                             } deriving (Show, Generic)

$(validatable [''ReadableString, ''MultiReadable, ''MultiLang, ''PersonForm])

-- {
--     "first_name": {
--         "en": {
--             "text": "Takuma",
--             "reading": "Takuma"
--         },
--         "ja": {
--             "text": "琢磨",
--             "reading": "タクマ"
--         }
--     },
--     "middle_name": {
--         "en": {
--             "text": "",
--             "reading": ""
--         },
--         "ja": {
--             "text": "",
--             "reading": ""
--         }
--     },
--     "last_name": {
--         "en": {
--             "text": "Iwasa",
--             "reading": "Iwasa"
--         },
--         "ja": {
--             "text": "岩佐",
--             "reading": "イワサ"
--         }
--     },
--     "description": {
--         "en": "CEO",
--         "ja": "代表取締役"
--     },
--     "typetalk_name": "iwasa"
-- }
-- insert into person (first_name, middle_name, last_name, description, notifications) values ('(Takuma,"琢磨",Takuma,"たくま")','("","","","")','(Iwasa,"岩佐",Iwasa,"いわさ")','("CEO","CEO")','{}')

create' :: SiteContext
        -> PersonForm'
        -> Action Person
create' sc form = do
    case validate form of
        Nothing -> do
            --throwError $ errorFor err400 (APIError 400 (errorsOf form)) sc
            throw err400
        Just f -> do
            (person, _) <- withContext @'[DB] sc $ do
                createPerson (buildPerson f)
            return person

buildPerson :: PersonForm
            -> Person
buildPerson f = Model (
    #id @= 0
 <: #first_name @= Label { en_label = (text . enN . first_name) f
                         , mb_label = (text . jaN . first_name) f
                         , en_reading = (rdg . enN . first_name) f
                         , mb_reading = (rdg . jaN . first_name) f
                         }
 <: #middle_name @= Label { en_label = (text . enN . middle_name) f
                          , mb_label = (text . jaN . middle_name) f
                          , en_reading = (rdg . enN . middle_name) f
                          , mb_reading = (rdg . jaN . middle_name) f
                          }
 <: #last_name @= Label { en_label = (text . enN . last_name) f
                        , mb_label = (text . jaN . last_name) f
                        , en_reading = (rdg . enN . last_name) f
                        , mb_reading = (rdg . jaN . last_name) f
                        }
 <: #description @= Lang { en = (enD . desc) f
                         , mb = (jaD . desc) f
                         }
 <: #notifications @= UTF8.toString (encode (Notifications { type_talk = Just (TypeTalk (typetalk_name f)) }))
 <: #nickname @= Label { en_label = (text . enN . nn) f
                       , mb_label = (text . jaN . nn) f
                       , en_reading = (rdg . jaN . nn) f
                       , mb_reading = (rdg . jaN . nn) f
                       }
 <: emptyRecord
 ) :: Person
    where
        enN = en :: MultiReadable -> ReadableString
        jaN = ja :: MultiReadable -> ReadableString
        enD = en :: MultiLang -> String
        jaD = ja :: MultiLang -> String
        rdg = reading :: ReadableString -> String
        desc = description :: PersonForm -> MultiLang
        nn = nickname :: PersonForm -> MultiReadable