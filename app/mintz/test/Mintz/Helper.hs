{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

module Mintz.Helper where

import GHC.TypeLits
import Data.Proxy
import Control.Monad
import Control.Monad.Writer
import Data.Model.Graph
import Database.ORM
import Database.ORM.Select
import Database.ORM.Utility

-- | This type holds IO actions launched before and after external IO action.
data PreSaveIO a r = forall s.
    PreSaveIO { preSave :: IO s -- ^ This action is launched before external action and saves a value.
              , genArg :: s -> IO a -- ^ Generates a value for the first argument of @execAfter@ by saved value.
              , execAfter :: a -> r -> IO () -- ^ IO action launched after extrenal action.
                                             --   @a@ is generated by @genArg@. @r@ is the result of external action.
              }

-- | Existential type of @PreSaveIO a r@ used to store their objects into a list.
data PreSaveIO' r = forall a. PreSaveIO' { preSaveIO :: PreSaveIO a r }

-- | Executes @PreSaveIO@s on the execution of an IO action.
runPreSave :: IO r -- ^ IO action.
           -> Writer [PreSaveIO' r] a -- ^ IO actions executed before and after the first argument.
           -> IO a -- ^ Value returned by @Writer@.
runPreSave f w = do
    let (v, psio) = runWriter w
    seq <- prepareIOs psio
    r <- f
    sequence_ $ map (\s -> s r) seq
    return v

prepareIOs :: [PreSaveIO' r]
           -> IO [r -> IO ()]
prepareIOs [] = return []
prepareIOs (PreSaveIO' (PreSaveIO {..}) : cs) = do
    s <- preSave
    let e = \r -> genArg s >>= (flip execAfter) r
    base <- prepareIOs cs
    return $ e : base

-- ----------------------------------------------------------------
-- DB checkers
-- ----------------------------------------------------------------

-- | Check DB with the result of preceding IO action.
check :: (WithDB db)
      => IO a -- ^ Preceding IO action.
      -> (a -> t -> IO ()) -- ^ Checking operation.
      -> Writer [PreSaveIO' t] () -- ^ @Writer@ accumulating @PreSaveIO@.
check pre f = do
    tell [PreSaveIO' (PreSaveIO { preSave = pre
                                , genArg = return
                                , execAfter = \a t -> f a t
                                })]

-- | Another version of @check@ having no preceding IO action.
check' :: (WithDB db)
       => (t -> IO ()) -- ^ Checking operation.
       -> Writer [PreSaveIO' t] () -- ^ @Writer@ accumelating @PreSaveIO@.
check' f = check (return ()) $ \_ t -> f t

-- | Check DB with the difference of the number of rows before and after testing operation.
diff :: forall r ts t g' r' db. (
        WithDB db
      , g' ~ ForCount (Graph r)
      , r' ~ (=*) r
      , RecordWrapper r
      , RecordWrapper r'
      , KnownNat (Length (EdgeTypes g' r'))
      , SelectNodes g' r' (EdgeTypes g' r')
      , ElemIndexes ts (EdgeTypes g' r'))
     => Condition ts -- ^ Conditions to determine rows to count.
     -> (Integer -> t -> IO ()) -- ^ Checking operation.
     -> Writer [PreSaveIO' t] () -- ^ @Writer@ accumelating @PreSaveIO@.
diff conds f = do
    tell [PreSaveIO' (PreSaveIO { preSave = cnt
                                , genArg = \before -> (+ (-before)) <$> cnt
                                , execAfter = \a t -> f a t
                                })]
    where
        cnt = countTable @r conds

-- | Compare record sets obtained by a query before and after testing operation.
compare :: forall r ts us t db. (WithDB db, RecordWrapper r, ElemIndexes ts '[r], ElemIndexes us '[r])
        => Condition ts -- ^ Conditions to determine records.
        -> OrderBy us -- ^ Ordering criteria to sort records.
        -> LimitOffset -- ^ Range of records.
        -> ([r] -> [r] -> t -> IO ()) -- ^ Checking operation.
        -> Writer [PreSaveIO' t] () -- ^ @Writer@ accumelating @PreSaveIO@.
compare conds sorts lo f = do
    tell [PreSaveIO' (PreSaveIO { preSave = get
                                , genArg = \before -> (before,) <$> get
                                , execAfter = \(b, a) t -> f b a t
                                })]
    where
        get :: IO [r]
        get = selectNodes (Proxy :: Proxy (Graph r)) (Proxy :: Proxy r) conds sorts lo >>= return . values 

-- | Compare a pair of records obtained by a query before and after testing operation.
compareOne :: forall r ts us t db. (WithDB db, RecordWrapper r, ElemIndexes ts '[r], ElemIndexes us '[r])
           => Condition ts -- ^ Conditions to determine records.
           -> OrderBy us -- ^ Ordering criteria to sort records.
           -> LimitOffset -- ^ Range of records.
           -> (r -> r -> t -> IO ()) -- ^ Checking operation.
           -> Writer [PreSaveIO' t] () -- ^ @Writer@ accumelating @PreSaveIO@.
compareOne conds sorts lo f = Mintz.Helper.compare conds sorts lo $ \bs as t -> f (bs !! 0) (as !! 0) t