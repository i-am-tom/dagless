{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Data.HListF
Description : An HList indexed by the context around every value within.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

A regular 'HList' contains a number of values of varying types, which are part
of the type-level index. An HListF extends this idea by specifying a context in
which all the values are wrapped. For example, notice here that our functor is
@[]@, which is the first index of the 'HListF' type. The second is the list of
@[]@-wrapped types that exist /within/ the list:

>>> :t [0 ..] :$> [True, False] :$> [()] :$> HNilF
[0 ..] :$> [True, False] :$> [()] :$> HNilF
  :: (Num x, Enum x) => HListF [] '[x, Bool, ()]
-}
module Data.HListF where

import Data.HList (HList (..))
import Data.Kind  (Constraint, Type)

-- | A heterogeneous list of context-wrapped values, indexed by their types.
-- The first index is the context in which these values are wrapped.

data HListF (f :: Type -> Type) (xs :: [Type]) where
  HNilF ::                       HListF f '[     ]
  (:$>) :: f x -> HListF f xs -> HListF f (x ': xs)

infixr 3 :$>

-- | Transform an 'HList' into an 'HListF' by factoring out some common
-- context. If all the values in the 'HList' are wrapped in some context @f@,
-- then it is isomorphic to some @HListF f@.
--
-- >>> :t factor @[] ([0 ..] :> [True, False] :> [()] :> HNil)
-- factor @[] ([0 ..] :> [True, False] :> [()] :> HNil)
--   :: (Num a, Enum a) => HListF [] '[a, Bool, ()]

class FactorOut (f :: Type -> Type) (is :: [Type]) (os :: [Type])
    | f is -> os, f os -> is where
  factor :: HList is -> HListF f os

instance FactorOut f '[] '[] where
  factor HNil = HNilF

instance FactorOut f xs ys => FactorOut f (f x ': xs) (x ': ys) where
  factor (x :> xs) = x :$> factor xs

-- | 'FactorIn' is a bad name for the opposite of 'FactorOut': here, we take an
-- 'HListF' and turn it into an 'HList' by "refactoring" the @f@ into each
-- type. It's not particularly exciting.
--
-- >>> :t refactor ([0 ..] :$> [True, False] :$> [()] :$> HNilF)
-- refactor ([0 ..] :$> [True, False] :$> [()] :$> HNilF)
--   :: (Num x, Enum x) => HList '[[x], [Bool], [()]]

class FactorIn (f :: Type -> Type) (is :: [Type]) (os :: [Type])
    | f is -> os, f os -> is where
  refactor :: HListF f is -> HList os

instance FactorIn f '[] '[] where
  refactor HNilF = HNil

instance FactorIn f xs ys => FactorIn f (x ': xs) (f x ': ys) where
  refactor (x :$> xs) = x :> refactor xs

-- | Pluck a type from an 'HListF'. The resultant value will be wrapped in the
-- context indexing the 'HListF', and the constraint ensures that it's total.
--
-- >>> pluckF @Bool ([0 ..] :$> [True, False] :$> [()] :$> HNilF)
-- [True,False]

class (x :: Type) `PluckedFromF` (xs :: [Type]) where
  pluckF :: HListF f xs -> f x

instance x `PluckedFromF` (x ': xs) where
  pluckF (head :$> _) = head

instance {-# OVERLAPPABLE #-} x `PluckedFromF` xs
    => x `PluckedFromF` (y ': xs) where
  pluckF (_ :$> tail) = pluckF tail

-- | Fold an HListF according to some constraint. Note that we constrain the
-- inner value, _not_ the context-wrapped value.
--
-- >>> foldF @Show (fmap show) ([0 .. 5] :$> [True, False] :$> [()] :$> HNilF)
-- ["0","1","2","3","4","5","True","False","()"]

class (c :: Type -> Constraint) `FoldsF` (xs :: [Type]) where
  foldF :: Monoid m => (forall x. c x => f x -> m) -> HListF f xs -> m

instance anything `FoldsF` '[] where
  foldF _ HNilF = mempty

instance (c x, c `FoldsF` xs) => c `FoldsF` (x ': xs) where
  foldF f (x :$> xs) = f x <> foldF @c f xs
