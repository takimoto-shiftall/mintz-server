{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.Model.Models where

import Control.Lens hiding ((:>))
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.List as L
import Data.Maybe (maybe)
import Data.Aeson
import Data.Extensible
import Database.ORM
import Mintz.Model.Types
import Mintz.Settings (db)

$(declareColumns db "person" "Person'")
$(declareColumns db "company" "Company'")
$(declareColumns db "employment" "Employment'")
$(declareColumns db "department" "Department'")
$(declareColumns db "employee_department" "EmployeeDepartment'")

type Person = "person" :++ Record Person'

fullName :: Person
         -> LangType
         -> String
fullName person t = L.intercalate " " $ filter (/= "") $ map (lang t) [view #first_name r, view #middle_name r, view #last_name r]
    where
        r = getRecord person
        lang EN = en_label
        lang MB = mb_label

mbName :: Person
       -> String
mbName = flip fullName $ MB

typeTalkName :: (RecordWrapper p, Associate "notifications" String (RW'Type p))
             => p
             -> Maybe String
typeTalkName person = let ns = decode $ C8.pack $ view #notifications (getRecord person) 
                      in name <$> (ns >>= type_talk)
