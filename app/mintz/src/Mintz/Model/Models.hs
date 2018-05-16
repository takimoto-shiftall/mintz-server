{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

module Mintz.Model.Models where

import Control.Lens hiding ((:>))
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
