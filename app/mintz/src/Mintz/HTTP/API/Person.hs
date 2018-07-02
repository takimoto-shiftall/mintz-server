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

type PersonAPI = "person" :> Use LinkSettings :>
               ( QueryParam "limit" Int
                          :> QueryParam "offset" Int
                          :> QueryParam "lang" String
                          :> Get '[JSON] Persons
            :<|> ReqBody '[JSON] PersonForm' :> Post '[JSON] ()
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

personAPI sc link = index' sc link
               :<|> create' sc link

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

create' :: SiteContext
        -> LinkSettings
        -> PersonForm'
        -> Action ()
create' sc link form = do
    case validate form of
        Nothing -> do
            --throwError $ errorFor err400 (APIError 400 (errorsOf form)) sc
            throw err400
        Just f -> do
            (person, _) <- withContext @'[DB] sc $ do
                createPerson (buildPerson f)
            return ()

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
                       , en_reading = (rdg . enN . nn) f
                       , mb_reading = (rdg . jaN . nn) f
                       }
 <: #display_order @= 0
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