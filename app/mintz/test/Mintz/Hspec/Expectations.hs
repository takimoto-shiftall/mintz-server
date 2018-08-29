{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Mintz.Hspec.Expectations where

import GHC.Stack
import Data.Proxy
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Test.Hspec

class ExpectFrom s (key :: k) t where
    expectFrom :: s -> Proxy key -> t

class ExpectFromBoth a e k where
    expectActual :: a -> Proxy e -> Proxy k -> BeAs k
    expectExpected :: e -> Proxy a -> Proxy k -> BeAs k

type ActualExpectations actual a = ReaderT actual IO a

shouldBeAs :: forall k actual expected. (
              HasCallStack
            , ExpectFromBoth actual expected k
            , Eq (BeAs k), Show (BeAs k))
           => expected
           -> ActualExpectations actual ()
shouldBeAs e = do
    a <- ask
    lift $ withFrozenCallStack $ (expectActual a (Proxy :: Proxy expected) (Proxy :: Proxy k))
                                 `shouldBe`
                                 (expectExpected e (Proxy :: Proxy actual) (Proxy :: Proxy k))

data key #> t
data t <# key

type family BeAs k :: * where
    BeAs (_ #> t <# _) = t
    BeAs (_ #> t) = t
    BeAs (t <# _) = t
    BeAs t = t

instance {-# OVERLAPS #-} ExpectFrom s s s where
    expectFrom = const

instance {-# OVERLAPPABLE #-} (ExpectFrom a k t, ExpectFrom e t t) => ExpectFromBoth a e (k #> t) where
    expectActual v _ _ = expectFrom v (Proxy :: Proxy k)
    expectExpected v _ _ = expectFrom v (Proxy :: Proxy t)

instance (ExpectFrom a t t, ExpectFrom e k t, BeAs (t <# k) ~ t) => ExpectFromBoth a e (t <# k) where
    expectActual v _ _ = expectFrom v (Proxy :: Proxy t)
    expectExpected v _ _ = expectFrom v (Proxy :: Proxy k)

instance {-# OVERLAPPABLE #-} (ExpectFrom a t t, ExpectFrom e t t, BeAs t ~ t) => ExpectFromBoth a e t where
    expectActual v _ = expectFrom v
    expectExpected v _ = expectFrom v

instance ( ExpectFrom a ak t, ExpectFrom e ek t
         ) => ExpectFromBoth a e (ak #> t <# ek) where
    expectActual v _ _ = expectFrom v (Proxy :: Proxy ak)
    expectExpected v _ _ = expectFrom v (Proxy :: Proxy ek)