{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Mintz.Site.Person where

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
import Database.ORM
import Data.Template
import Ext.Servant.Action
import Ext.Servant.Context
import Ext.Servant.Template
import Mintz.Model.Models
import Mintz.Model.Types
import Mintz.Service.Person
import Mintz.Settings
import Ext.Servant.Validation

{-# ANN module (Hide "Data.ByteString.Lazy.UTF8") #-}

defaultListLength = 100

data PersonForm = PersonForm { first_name_en :: String
                             , first_name_ja :: String
                             , middle_name_en :: String
                             , middle_name_ja :: String
                             , last_name_en :: String
                             , last_name_ja :: String
                             , description_en :: String
                             , description_ja :: String
                             , typetalk_name :: String
                             } deriving (Show, Generic)

$(validatable [''PersonForm])

[tmpld| PersonCreate template/person/create.html (Maybe PersonForm') |]
[tmpld| PersonIndex template/person/index.html [Person] |]
[tmpld| PersonConfirmCreate template/person/create-confirm.html PersonForm |]

-- cache_PersonIndex = unsafePerformIO $ newIORef Nothing
-- cache_PersonCreate = unsafePerformIO $ newIORef Nothing

type PersonAPI = "person" :>
               ( QueryParam "limit" Int
                    :> QueryParam "offset" Int
                    :> Get '[PersonIndex] [Person]
            :<|> "form" :> Get '[PersonCreate] (Maybe PersonForm')
            :<|> "confirm" :> ReqBody '[FormUrlEncoded] PersonForm' :> Post '[HTML] Renderer
            :<|> ReqBody '[FormUrlEncoded] PersonForm' :> Post '[HTML] Renderer
               )

personAPI sc = index' sc
          :<|> inputCreate' sc
          :<|> confirmCreate' sc
          :<|> create' sc

index' :: SiteContext
       -> Maybe Int
       -> Maybe Int
       -> Action [Person]
index' sc limit offset = do
    (persons, _) <- withContext @'[DB] sc $ do
        listPersons (maybe defaultListLength id limit) (maybe 0 id offset)
    return persons

inputCreate' :: SiteContext
             -> Action (Maybe PersonForm')
inputCreate' sc = return Nothing

confirmCreate' :: SiteContext
               -> PersonForm'
               -> Action Renderer
confirmCreate' sc form = do
    maybe back return $ validate form >>= \f -> do
        let person = buildPerson f
        return $ Renderer (Proxy :: Proxy PersonConfirmCreate) f
    where
        back :: Action Renderer
        back = return $ Renderer (Proxy :: Proxy PersonCreate) (Just form)

create' :: SiteContext
        -> PersonForm'
        -> Action Renderer
create' sc form = do
    case validate form of
        Nothing -> back
        Just f -> do
            (person, _) <- withContext @'[DB] sc $ do
                createPerson (buildPerson f)
            return $ Renderer (Proxy :: Proxy PlainText)
                              (addHeader "/person" () :: Headers '[Header "Location" String] ())
    where
        back :: Action Renderer
        back = return $ Renderer (Proxy :: Proxy PersonCreate) (Just form)

buildPerson :: PersonForm
            -> Person
buildPerson f = Model (
    #id @= 0
 <: #first_name @= Label { en_label = first_name_en f
                         , mb_label = first_name_ja f
                         , en_reading = ""
                         , mb_reading = ""
                         }
 <: #middle_name @= Label { en_label = middle_name_en f
                          , mb_label = middle_name_ja f
                          , en_reading = ""
                          , mb_reading = ""
                          }
 <: #last_name @= Label { en_label = last_name_en f
                        , mb_label = last_name_ja f
                        , en_reading = ""
                        , mb_reading = ""
                        }
 <: #description @= Lang { en = description_en f
                         , mb = description_ja f
                         }
 <: #notifications @= UTF8.toString (encode (Notifications { type_talk = Just (TypeTalk (typetalk_name f)) }))
 <: emptyRecord
 ) :: Person