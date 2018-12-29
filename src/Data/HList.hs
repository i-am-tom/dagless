{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Data.HList
Description : A nice, neat HList.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

This module defines a heterogeneous list type. Any types can be stored in the
list, and the type itself is indexed by the types contained within. This module
also defines functions for extracting values /and/ folding an HList in which
all inhabitant types implement some constraint.
-}
module Data.HList where

import Data.Kind (Constraint, Type)
import Prelude   hiding (foldMap)

-- | A heterogeneous list, indexed by the types that it contains. We define the
-- ':>' notation for @Cons@, which hopefully makes uses a bit neater.

data HList (xs :: [Type]) where
  HNil ::                  HList '[     ]
  (:>) :: x -> HList xs -> HList (x ': xs)

infixr 3 :>

-- | Pluck a type from an HList. The constraint ensures that the type exists
-- within the list, and thus it is a total function.
--
-- >>> :set -XTypeApplications
-- >>> let example = True :> 3 :> "hello" :> HNil
--
-- >>> pluck @Bool example
-- True
--
-- >>> pluck @String example
-- "hello"
--
-- >>> pluck @(()) example
-- ...
-- ... No instance for (PluckedFrom ...)
-- ...

class (x :: Type) `PluckedFrom` (xs :: [Type]) where
  pluck :: HList xs -> x

instance x `PluckedFrom` (x ': xs) where
  pluck (head :> _) = head

instance {-# OVERLAPPABLE #-} x `PluckedFrom` xs
    => x `PluckedFrom` (y ': xs) where
  pluck (_ :> tail) = pluck tail

-- | Fold an HList. Assuming all the elements of the list satisfy some
-- constraint, we should be able to "fold" over the list according to some
-- monoid value produced using that constraint.
-- 
-- >>> let example = True :> 3 :> "hello" :> HNil
-- >>> foldMap @Show (pure @[] . show) example
-- ["True","3","\"hello\""]
--
-- We can also fold over a homogeneous HList using an equality constraint,
-- allowing us to recover the traditional list folds:
--
-- >>> import Data.Monoid
-- >>> foldMap @((~) Int) Sum (3 :> 2 :> 1 :> HNil)
-- Sum {getSum = 6}
--
-- Finally, we can use 'Data.Coerce.coerce' for some truly interesting
-- operations:
--
-- >>> import Data.Coerce
-- >>> let example = Any False :> False :> All True :> HNil
-- >>> foldMap @(Coercible Any) (coerce @_ @Any) example
-- Any {getAny = True}

class (c :: Type -> Constraint) `Folds` (xs :: [Type]) where
  foldMap :: Monoid m => (forall x. c x => x -> m) -> HList xs -> m

instance anything `Folds` '[] where
  foldMap _ HNil = mempty

instance (c x, c `Folds` xs) => c `Folds` (x ': xs) where
  foldMap f (x :> xs) = f x <> foldMap @c f xs
