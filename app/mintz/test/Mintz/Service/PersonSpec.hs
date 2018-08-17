{-# LANGUAGE OverloadedLabels #-}

module Mintz.Service.PersonSpec where

import Test.Hspec
import Data.IORef
import Control.Lens
import Data.Extensible
import Data.Resource
import Data.Model.Graph
import Database.ORM
import Mintz.Model.Types
import Mintz.Model.Models
import Mintz.Settings
import Mintz.Service.Person
import Mintz.Hspec.IO
import Mintz.Hspec.Database

type PersonGraph = Graph Person

spec :: Spec
spec = do
    describe "Person creation" $ do
        it "insert a person" $ do
            db' <- newResource db
            let resources = db' `RCons` RNil

            withContext' @'[DB] resources $ do
                let person = Model ( #id @= 0
                                  <: #first_name @= Label "iwasa" "岩佐" "iwasa" "いわさ"
                                  <: #middle_name @= Label "" "" "" ""
                                  <: #last_name @= Label "takuma" "琢磨" "takuma" "たくま"
                                  <: #description @= Lang "president" "社長"
                                  <: #notifications @= "{\"nickname\": \"iwasa\"}"
                                  <: #nickname @= Label "iwasa san" "岩佐さん" "iwasa san" "いわささん"
                                  <: #display_order @= 0
                                  <: emptyRecord
                                   ) :: (=+)Person
                createPerson person
                `runPrecedable` do
                    inc @Person (..?) 1
                    new1 @Person $ \p -> (p ~/ shrink :: Person :^- '["display_order"])
