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

typeTalkName :: (RecordWrapper p, Associate "notifications" String (RW'Type p))
             => p
             -> Maybe String
typeTalkName person = let ns = decode $ C8.pack $ view #notifications (getRecord person) 
                      in name <$> (ns >>= type_talk)
