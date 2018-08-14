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

type PersonGraph = Graph Person

spec :: Spec
spec = do
    describe "Person creation" $ do
        it "insert a person" $ do
            db' <- newResource db
            let resources = db' `RCons` RNil

            withContext @'[DB] resources $ do
                let person = Model ( #id @= 0
                                  <: #first_name @= Label "iwasa" "岩佐" "iwasa" "いわさ"
                                  <: #middle_name @= Label "" "" "" ""
                                  <: #last_name @= Label "takuma" "琢磨" "takuma" "たくま"
                                  <: #description @= Lang "president" "社長"
                                  <: #notifications @= "{\"nickname\":\"iwasa\"}"
                                  <: emptyRecord
                                   ) :: Person
                let (graph, _) = person +< (newGraph :: PersonGraph)
                restoreGraph graph

            return ()

data Checker a = forall s. Checker { memory :: IO s
                                   , reuse :: s -> IO a
                                   }

instance Monad Checker where
    -- (>>=) :: Checker a -> (a -> Checker b) -> Checker b
    -- 
    c@(Checker {..}) >>= f = Checker
        where
            m = 

-- Checkerは作られた時点でIOアクションによりデータをメモリに積む
-- あとで取り出す値とは型が違う。

c1 >>= \a -> c2
-- c1のreuseにより値を取り出す。
(>>=) :: Checker a -> (a -> Checker b) -> Checker b
(>>=) c = do
    reuse (memory c1)

oldOne :: forall r. (RecordWrapper r)
       => Checker r
oldOne t = Checker m (return . id)
    where
        m = [sql| SELECT * ... | ]

data CheckerItem = forall s a. CheckerItem { pre :: IO s
                                           , conv :: s -> IO a
                                           , exec :: a -> IO ()
                                           }

getExec :: CheckerItem -> IO ()
getExec (CheckerItem {..}) = pre >>= conv >>= exec

check :: CheckerItem -> IO ()
check c = do
    let ex = getExec c
    print "abc"
    ex

-- (1-1, 1-2)
-- (2-1, 2-2)
-- (3-1, 3-3)
--
-- 1-1 -> 2-1 -> 3-1 -> 1-2 -> 2-2 -> 3-3
-- c1 >>= <1-2> -> c2
-- Checker [1-1, 1-2] 

data Checker (as :: [*]) = Checker { memory :: HList as
                                   , runChecker :: HList (TMap IO as)
                                   }

-- checkIO :: IO a -> Checker a
-- 
-- newOne :: (RecordWrapper r) => Checker r
-- diff :: forall r. (RecordWrapper r) => Checker Integer
--
-- -- Checker Person
-- p <- checkFor $ do
--     fst <$> withContext @'[DB] (createPerson m)
-- -- Checker Person
-- p' <- newOne @Person
-- -- Checker Integer
-- d <- diff @Person
-- -- Checker ()
-- check $ do
--    view #id p `shouldBe` view #id p'
--    d `shouldBe` 1

-- checkIO (..) >>= \p -> do  
--     newOne @Person
--  >>= \p' -> do
--     view #id p `shouldBe` view #id p'
--  >>= \_ -> do
--     diff @Person `
--  >>= \d -> do
--     d `shouldBe` 1