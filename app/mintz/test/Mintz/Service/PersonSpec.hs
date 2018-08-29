{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Mintz.Service.PersonSpec where

import Test.Hspec
import Data.IORef
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Lens
import Data.Extensible
import Data.Resource
import Data.Model.Graph
import Database.ORM
import Mintz.Model.Types
import Mintz.Model.Models
import Mintz.Service.Person
import Mintz.Hspec.IO
import Mintz.Hspec.Expectations
import Mintz.Hspec.Database
import Mintz.Hspec.Settings (db, DB)

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
                    new1 @Person $ \p -> do
                        shouldBeAs @(Person :^- '["display_order"]) p
                        order <- asks $ view #display_order
                        liftIO $ order `shouldBe` 3


