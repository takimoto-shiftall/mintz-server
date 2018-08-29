{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Mintz.Hspec.Database.Fixture where

import GHC.Exts
import GHC.TypeLits
import GHC.OverloadedLabels
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Proxy
import qualified Data.Char as C
import Language.Haskell.TH
import Data.Extensible
import Database.ORM
import Database.ORM.Utility

-- ----------------------------------------------------------------
-- Base definitions for fixture.
-- ----------------------------------------------------------------

data Fixture (key :: Symbol) (fs :: [Assoc Symbol *])

data FixNum n (a :: Nat)
data FixStr (a :: Symbol)

type family FixtureOf (as :: [*]) :: [Assoc Symbol *] where
    FixtureOf '[] = '[]
    FixtureOf (Fixture _ fs ': as) = fs
    FixtureOf (_ ': as) = FixtureOf as

type family FixtureKey (as :: [*]) :: Symbol where
    FixtureKey '[] = ""
    FixtureKey (Fixture key _ ': as) = key
    FixtureKey (_ ': as) = FixtureKey as

class FixtureType f where
    type FixtureBase f :: *
    fixture :: (Monad m)
            => Proxy f
            -> String
            -> Int
            -> m (FixtureBase f)

instance (KnownNat a, Num n) => FixtureType (FixNum n a) where
    type FixtureBase (FixNum n a) = n
    fixture _ _ _ = pure $ fromInteger $ natVal (Proxy :: Proxy a)

instance (KnownSymbol a) => FixtureType (FixStr a) where
    type FixtureBase (FixStr a) = String
    fixture _ _ _ = pure $ symbolVal (Proxy :: Proxy a)

class GetFixture (fs :: [Assoc Symbol *]) (kv :: Assoc Symbol *) where
    getFixture :: (Monad m)
               => Proxy kv
               -> Proxy fs
               -> String
               -> Int
               -> m (AssocValue kv)

instance (v ~ AssocValue kv, FixtureType v, FixtureBase v ~ v) => GetFixture '[] kv where
    getFixture _ _ key index = fixture (Proxy :: Proxy v) key index

instance (FixtureType w, FixtureBase w ~ v) => GetFixture ((k ':> w) ': fs) (k ':> v) where
    getFixture _ _ key index = fixture (Proxy :: Proxy w) key index

instance {-# OVERLAPPABLE #-} (GetFixture fs (k ':> v)) => GetFixture ((j ':> w) ': fs) (k ':> v) where
    getFixture p _ key index = getFixture p (Proxy :: Proxy fs) key index


generateFixture :: forall m r fs. (Monad m
                   , RecordWrapper r
                   , fs ~ FixtureOf (RW'Spec r)
                   , Forall (GetFixture fs) (RW'Type r) 
                   )
                => Proxy r
                -> String
                -> Int
                -> m r
generateFixture _ key index = wrapRecord <$> hgenerateFor (Proxy :: Proxy (GetFixture fs))
                                                          (\m -> Field . pure <$> gen m)
    where
        gen :: forall xs kv. (GetFixture fs kv) => Membership xs kv -> m (AssocValue kv)
        gen _ = getFixture (Proxy :: Proxy kv) (Proxy :: Proxy fs) key index


-- ----------------------------------------------------------------
-- TH fixtures.
-- ----------------------------------------------------------------

newtype FixtureTypeName = FixtureTypeName String

instance (KnownSymbol x) => IsLabel (x :: Symbol) FixtureTypeName where
    fromLabel = FixtureTypeName $ symbolVal (Proxy :: Proxy x)

fixtureType :: FixtureTypeName
            -> Name
            -> ExpQ
            -> Q [Dec]
fixtureType (FixtureTypeName (h : n)) tn exp = do
    let name = mkName (C.toUpper h : n)
    dd <- dataD (cxt []) name [] Nothing [] []
    di <- [d|
            instance FixtureType $(conT name) where
                type FixtureBase $(conT name) = $(conT tn)
                fixture _ = $(exp)
        |]
    return $ dd : di

-- ----------------------------------------------------------------
-- Fixture generation state.
-- ----------------------------------------------------------------

type FixtureState m a = StateT (M.Map String Int) m a

newtype FixtureStateKey = FixtureStateKey String

instance (KnownSymbol x) => IsLabel (x :: Symbol) FixtureStateKey where
    fromLabel = FixtureStateKey $ symbolVal (Proxy :: Proxy x)

runFixture :: (Monad m)
           => FixtureState m a
           -> m a
runFixture s = fst <$> runStateT s M.empty

gen :: forall r m. (RecordWrapper r
     , Monad m
     , KnownSymbol (FixtureKey (RW'Spec r))
     , Forall (GetFixture (FixtureOf (RW'Spec r))) (RW'Type r))
    => FixtureStateKey
    -> Int
    -> FixtureState m [r]
gen (FixtureStateKey key) n = do
    history <- get
    let index = maybe 0 id (history M.!? mapKey)
    records <- lift $ forM [0..n-1] $ \i -> generateFixture (Proxy :: Proxy r) key (index + i)
    modify (M.alter (const $ Just $ index + n) mapKey)
    return $ records
    where
        -- [table name]/[fixture key]/[state key]
        mapKey :: String
        mapKey = getName (Proxy :: Proxy r) ++ "/" ++ symbolVal (Proxy :: Proxy (FixtureKey (RW'Spec r))) ++ "/" ++ key