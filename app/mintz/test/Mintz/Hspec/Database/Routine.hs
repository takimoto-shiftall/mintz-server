{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Mintz.Hspec.Database.Routine where

import GHC.Stack
import GHC.TypeLits
import Test.Hspec
import Data.Extensible
import Data.Resource
import Data.Model.Graph
import Database.ORM
import Database.ORM.Select
import Database.ORM.Utility
import Mintz.Hspec.IO
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

new :: forall r r' t db. (
       WithDB db
     , RecordWrapper r, Identifiable r, RecordWrapper r', Identifiable r'
     , Include (RW'Type r) (RW'Type r')
     , Forall (Instance1 Show (Field Identity)) (RW'Type r')
     , Forall (Instance1 Eq (Field Identity)) (RW'Type r'))
    => (t -> [r'])
    -> Precedable t ()
new recs = diff @r $ \(ns, ds) t -> withFrozenCallStack $ map (shrink . getRecord) ns `shouldBe` map getRecord (recs t)

new1 :: forall r r' t db. (
        WithDB db
      , RecordWrapper r, Identifiable r, RecordWrapper r', Identifiable r'
      , Include (RW'Type r) (RW'Type r')
      , Forall (Instance1 Show (Field Identity)) (RW'Type r')
      , Forall (Instance1 Eq (Field Identity)) (RW'Type r'))
     => (t -> r')
     -> Precedable t ()
new1 rc = new @r $ \t -> [rc t]
