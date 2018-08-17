{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

module Mintz.Hspec.Database.Precede where

import GHC.TypeLits
import Data.Proxy
import Control.Monad
import qualified Data.List as L
import Data.Resource
import Data.Model.Graph
import Database.ORM
import Database.ORM.Select
import Database.ORM.Utility
import Mintz.Hspec.IO

-- | Check DB with the difference of the number of rows before and after testing operation.
count :: forall r ts t g' r' db. (
         WithDB db
       , g' ~ ForCount (Graph r)
       , r' ~ (=*) r
       , RecordWrapper r
       , RecordWrapper r'
       , KnownNat (Length (EdgeTypes g' r'))
       , SelectNodes g' r' (EdgeTypes g' r')
       , ElemIndexes ts (EdgeTypes g' r'))
      => Condition ts -- ^ Conditions to determine rows to count.
      -> (Integer -> Integer -> t -> IO ()) -- ^ Checking operation.
      -> Precedable t () -- ^ Returns no value.
count conds f = precedeIO cnt (\before -> (before,) <$> cnt) (\(a, b) t -> f a b t)
    where
        cnt = countTable @r conds

-- | Compare record sets obtained by a query before and after testing operation.
comp :: forall r ts us t db. (WithDB db, RecordWrapper r, ElemIndexes ts '[r], ElemIndexes us '[r])
     => Condition ts -- ^ Conditions to determine records.
     -> OrderBy us -- ^ Ordering criteria to sort records.
     -> LimitOffset -- ^ Range of records.
     -> ([r] -> [r] -> t -> IO ()) -- ^ Checking operation.
     -> Precedable t () -- ^ Returns no value.
comp conds sorts lo f = precedeIO get (\before -> (before,) <$> get) (\(b, a) t -> f b a t)
    where
        get :: IO [r]
        get = selectNodes (Proxy :: Proxy (Graph r)) (Proxy :: Proxy r) conds sorts lo >>= return . values 

-- | Compare a pair of records obtained by a query before and after testing operation.
compOne :: forall r ts us t db. (WithDB db, RecordWrapper r, ElemIndexes ts '[r], ElemIndexes us '[r])
        => Condition ts -- ^ Conditions to determine records.
        -> OrderBy us -- ^ Ordering criteria to sort records.
        -> LimitOffset -- ^ Range of records.
        -> (r -> r -> t -> IO ()) -- ^ Checking operation.
        -> Precedable t () -- ^ Returns no value.
compOne conds sorts lo f = comp conds sorts lo $ \bs as t -> f (bs !! 0) (as !! 0) t

diff :: forall r t db. (WithDB db, RecordWrapper r, Identifiable r)
    => (([r], [r]) -> t -> IO ())
    -> Precedable t ()
diff f = precedeIO current newAndLost (\a t -> f a t)
    where
        current :: (WithDB db) => IO [r]
        current = do
            g <- selectNodes (Proxy :: Proxy (Graph r)) (Proxy :: Proxy r) (..?) (../) Nothing
            return (values g :: [r])

        newAndLost :: (WithDB db) => [r] -> IO ([r], [r])
        newAndLost old = do
            cur <- current
            let keys = getKeyNames (Proxy :: Proxy r)
            return (L.deleteFirstsBy ident cur old, L.deleteFirstsBy ident old cur)

