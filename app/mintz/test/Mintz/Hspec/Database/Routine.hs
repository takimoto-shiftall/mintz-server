{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module Mintz.Hspec.Database.Routine where

import GHC.Stack
import GHC.OverloadedLabels
import GHC.TypeLits
import Control.Monad.Reader
import Data.Proxy
import Test.Hspec
import Control.Lens hiding ((:>))
import Data.Extensible
import Data.Resource
import Data.Model.Graph
import Database.ORM
import Database.ORM.Select
import Database.ORM.Utility
import Mintz.Hspec.IO
import Mintz.Hspec.Expectations
import Mintz.Hspec.Database.Precede

-- ----------------------------------------------------------------
-- Insert:
--   - The number of rows is increased.
--   - Columns of inserted rows are correct.
-- Update:
--   - Columns of target row are changed.
--   - Other rows are not changed.
-- Delete:
--   - The number of rows is decreased.
--   - Deleted rows are correct. 
-- ----------------------------------------------------------------

inc :: forall r ts t g' r' db. (
       WithDB db
     , g' ~ ForCount (Graph r), r' ~ (=*) r, RecordWrapper r, RecordWrapper r'
     , KnownNat (Length (EdgeTypes g' r')), SelectNodes g' r' (EdgeTypes g' r'), ElemIndexes ts (EdgeTypes g' r'))
    => Condition ts -- ^ Conditions to determine rows to count.
    -> Integer
    -> Precedable t () -- ^ Returns no value.
inc conds i = count @r conds $ \a b _ -> withFrozenCallStack $ (b - a) `shouldBe` i

dec :: forall r ts t g' r' db. (
       WithDB db
     , g' ~ ForCount (Graph r), r' ~ (=*) r, RecordWrapper r, RecordWrapper r'
     , KnownNat (Length (EdgeTypes g' r')), SelectNodes g' r' (EdgeTypes g' r'), ElemIndexes ts (EdgeTypes g' r'))
    => Condition ts -- ^ Conditions to determine rows to count.
    -> Integer
    -> Precedable t () -- ^ Returns no value.
dec conds i = count @r conds $ \a b _ -> withFrozenCallStack $ (a - b) `shouldBe` i

--new :: forall r r' t db. (
--       WithDB db
--     , RecordWrapper r, Identifiable r, RecordWrapper r', Identifiable r'
--     , Include (RW'Type r) (RW'Type r')
--     , Forall (Instance1 Show (Field Identity)) (RW'Type r')
--     , Forall (Instance1 Eq (Field Identity)) (RW'Type r'))
--    => (t -> [r'])
--    -> Precedable t ()
--new recs = diff @r $ \(ns, ds) t -> withFrozenCallStack $ map (shrink . getRecord) ns `shouldBe` map getRecord (recs t)
--
--new1 :: forall r r' t db. (
--        WithDB db
--      , RecordWrapper r, Identifiable r, RecordWrapper r', Identifiable r'
--      , Include (RW'Type r) (RW'Type r')
--      , Forall (Instance1 Show (Field Identity)) (RW'Type r')
--      , Forall (Instance1 Eq (Field Identity)) (RW'Type r'))
--     => (t -> r')
--     -> Precedable t ()
--new1 rc = new @r $ \t -> [rc t]

new :: forall r r' t a db. (
       WithDB db
     , RecordWrapper r, Identifiable r)
    => (t -> ActualExpectations [r] ())
    -> Precedable t ()
new exp = diff @r $ \(ns, ds) t -> runReaderT (exp t) ns

new1 :: forall r r' t a db. (
        WithDB db
      , HasCallStack
      , RecordWrapper r, Identifiable r)
     => (t -> ActualExpectations r ())
     -> Precedable t ()
new1 exp = diff @r test
    where
        test :: ([r], [r]) -> t -> IO ()
        test (ns, _) t = do
            withFrozenCallStack $ length ns `shouldBe` 1
            runReaderT (exp t) (ns !! 0)

instance (t ~ (TableModel n r m as), RecordWrapper t, Forall (Instance1 Eq (Field Identity)) (RW'Type t)) => Eq (TableModel n r m as) where
    m1 == m2 = getRecord m1 == getRecord m2
instance (t ~ (ExtraModel xs as), RecordWrapper t, Forall (Instance1 Eq (Field Identity)) (RW'Type t)) => Eq (ExtraModel xs as) where
    m1 == m2 = getRecord m1 == getRecord m2

instance (t ~ TableModel n r m as, RecordWrapper t, RecordWrapper r', Include (RW'Type t) (RW'Type r'), Rewrap t r'
         ) => ExpectFrom (TableModel n r m as) r' r' where
    expectFrom r _ = r ~/ shrink

instance (RecordWrapper r, Associate k v (RW'Type r)) => ExpectFrom r (k :: Symbol) v where
    expectFrom r _ = view (itemAssoc (Proxy :: Proxy k)) (getRecord r)