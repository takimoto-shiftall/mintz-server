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
import qualified Data.List as L
import Text.Printf
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
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
            :<|> ReqBody '[JSON] PersonForm' :> Post '[JSON] Person
            :<|> Capture "id" Integer :>
                   ( ReqBody '[JSON] PersonForm' :> Put '[JSON] NoContent
                :<|> "icon" :> ReqBody '[OctetStream] BL.ByteString :> Post '[JSON] ()
                   )
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
               :<|> identified
    where
        identified pid = update' sc link pid
                    :<|> addIcon' sc link pid

index' :: SiteContext
       -> LinkSettings
       -> Maybe Int
       -> Maybe Int
       -> Maybe String
       -> Action Persons
index' sc link limit offset lang = do
    (persons, _) <- withContext @'[DB] sc $ do
        listPersons (maybe defaultListLength Prelude.id limit) (maybe 0 Prelude.id offset)
    icons <- mapM (findIcon $ icon_dir link) persons
    let lang' = maybe defaultLang Prelude.id lang
    return $ Persons $ map (model2Item link lang') $ zip persons icons

model2Item :: LinkSettings
           -> String
           -> (Person, Maybe FilePath)
           -> PersonItem
model2Item link lang (p, icon) =
    let r = getRecord p
        (nm, rdg) = nameAndReading p lang
        iconUrl = maybe (default_icon link) ((icon_url link ++)) icon
    in PersonItem { id = view #id r
                  , name = nm
                  , reading = rdg
                  , nickname = (labelOf lang) (view #nickname r)
                  , icon = iconUrl
                  , description = (langOf lang) (view #description r)
                  }

create' :: SiteContext
        -> LinkSettings
        -> PersonForm'
        -> Action Person
create' sc link form = do
    f <- validateOr400 sc form
    (person, _) <- withContext @'[DB] sc $ do
        createPerson (buildPerson f)
    return person

update' :: SiteContext
        -> LinkSettings
        -> Integer
        -> PersonForm'
        -> Action NoContent
update' sc link pid form = do
    f <- validateOr400 sc form
    withContext @'[DB] sc $ do
        let person = Model (
                let r = shrink $ getRecord $ buildPerson f
                in set #id pid r
                ) :: UpdatePerson
        updatePerson (Model (shrink $ getRecord $ buildPerson f) :: UpdatePerson)
    return NoContent

addIcon' :: SiteContext
         -> LinkSettings
         -> Integer
         -> BL.ByteString
         -> Action ()
addIcon' sc link pid icon = do
    (res, _) <- withContext @'[DB] sc $ createIcon (icon_dir link) pid icon
    if res then return ()
           else throw $ err404

nameAndReading :: Person -> String -> (String, String)
nameAndReading p "ja" = let r = getRecord p
                        in ( mb_label (view #last_name r) ++ " " ++ mb_label (view #first_name r)
                           , mb_reading (view #last_name r) ++ " " ++ mb_reading (view #first_name r)
                           )
nameAndReading p _ = let r = getRecord p
                     in ( L.intercalate " " $ map en_label [view #first_name r, view #middle_name r, view #last_name r]
                        , L.intercalate " " $ map en_reading [view #first_name r, view #middle_name r, view #last_name r]
                        )

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