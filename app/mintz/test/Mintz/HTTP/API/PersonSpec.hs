{-# LANGUAGE OverloadedLabels #-}

module Mintz.HTTP.API.PersonSpec where

import Test.Hspec
import qualified Test.Hspec.Wai as T
import Mintz.HTTP.App

spec :: Spec
spec = T.with app $ do
    describe "Create person" $ do
        it "Valid" $ 
            T.pending