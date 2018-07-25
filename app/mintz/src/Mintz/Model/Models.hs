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

$(declareModels db "person" "Person")
$(declareModels db "company" "Company")
$(declareModels db "employment" "Employment")
$(declareModels db "department" "Department")
$(declareModels db "employee_department" "EmployeeDepartment")
$(declareModels db "publish_log" "PublishLog")
$(declareModels db "called_person" "CalledPerson")

typeTalkName :: (RecordWrapper p, Associate "notifications" String (RW'Type p))
             => p
             -> Maybe String
typeTalkName person = let ns = decode $ C8.pack $ view #notifications (getRecord person) 
                      in name <$> (ns >>= type_talk)
