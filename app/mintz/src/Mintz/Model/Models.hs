{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.Model.Models where

import GHC.TypeLits
import Control.Lens hiding ((:>))
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.List as L
import Data.Maybe (maybe)
import Data.Aeson
import Data.Extensible
import Database.ORM
import Mintz.Model.Types
import Mintz.Settings (db)

-- version: 20180701_213452

$(declareColumns db "person" "Person'")
$(declareColumns db "company" "Company'")
$(declareColumns db "employment" "Employment'")
$(declareColumns db "department" "Department'")
$(declareColumns db "employee_department" "EmployeeDepartment'")
$(declareColumns db "publish_log" "PublishLog'")
-- TODO 外部キーだけだとkindが[*]になってしまう。
-- $(declareColumns db "called_person" "CalledPerson'")

type Person = "person" :++ Record Person'
type Person'S = "person" :## Record Person'

type PublishLog = "publish_log" :++ Record PublishLog'
type CalledPerson = "called_person" :++ Record ('[] :: [Assoc Symbol *])

fullName :: Person
         -> LangType
         -> String
fullName person EN =
    let r = getRecord person
    in L.intercalate " " $ filter (/= "") $ map en_label [view #first_name r, view #middle_name r, view #last_name r]
fullName person MB =
    let r = getRecord person
    in mb_label (view #last_name r) ++ " " ++ mb_label (view #first_name r)

mbName :: Person
       -> String
mbName = flip fullName $ MB

typeTalkName :: (RecordWrapper p, Associate "notifications" String (RW'Type p))
             => p
             -> Maybe String
typeTalkName person = let ns = decode $ C8.pack $ view #notifications (getRecord person) 
                      in name <$> (ns >>= type_talk)
