{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Data.HDagF
Description : A directed, acyclic graph expressed as a constrained list.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

DAGs are a tricky thing to express in a way that allows the compiler to help
you. Luckily for me, Twan van Laarhoven didn't just devote his a to lenses: in
https://www.twanvl.nl/blog/haskell/dependently-typed-dags, a nice approach to
this problem is discussed, using @Vector@ and @Fin@ to guarantee that a valid
index is given for every dependency.

This module extends this idea to heterogeneous DAGs. Of course, @Fin@ is no
longer a good enough index type, as we need to know /which/ child is being
indexed. To do this, 'HDagF' requires that all nodes be /unique/ types, and
thus we can unambiguously reference required nodes by their types.

Let's imagine we want to build a DAG to show which GHC language extensions are
implied by others. We'll think specifically about the little tree formed by
@TypeInType@:

@
  TypeInType
    |
    +-- PolyKinds
    |     |
    +---- +-- KindSignatures
    |
    +-- DataKinds
@

According to the docs, @TypeInType@ implies @PolyKinds@, @KindSignatures@, and
@DataKinds@. However, @PolyKinds@ also implies @KindSignatures@, so we have a
nice little DAG!

Let's first create some types:

>>> data DataKinds      = DataKinds
>>> data KindSignatures = KindSignatures
>>> data PolyKinds      = PolyKinds
>>> data TypeInType     = TypeInType

.. and switch on some extensions:

>>> :set -XDataKinds -XTypeApplications

Now, let's express it with an HDagF:

>>> :{
>>> typeInType
>>>   :: Applicative f
>>>   => HDagF f '[TypeInType, PolyKinds, DataKinds, KindSignatures]
>>> typeInType
>>>   = HNodeF @'[DataKinds, PolyKinds, KindSignatures] (\_ -> pure TypeInType)
>>>   . HNodeF @'[KindSignatures] (\_ -> pure PolyKinds)
>>>   . HNodeF @'[] (\_ -> pure DataKinds)
>>>   . HNodeF @'[] (\_ -> pure KindSignatures)
>>>   $ HEmptyF
>>> :}

So, it's a bit ugly, but hopefully parseable. We type-apply the dependencies of
the node we're creating, and then pass a function that performs some
computation involving those values. Better examples can be found in the 'test/'
directory.
-}
module Data.HDagF
  ( AreChildrenOf (..)

  , HDagF  (..)
  , HListF (..)

  , In
  , evaluate
  ) where

import Data.HListF (HListF (..))
import Data.Kind   (Type)

-- | Test for an element's presence within a type-level list.
--
-- >>> :set -XDataKinds -XTypeOperators
-- >>> :kind! Bool `In` '[]
-- Bool `In` '[] :: Bool
-- = 'False
--
-- >>> :kind! Bool `In` '[String, Int, Bool]
-- Bool `In` '[String, Int, Bool] :: Bool
-- = 'True

type family (x :: k) `In` (xs :: [k]) :: Bool where
  x `In` (x ': xs) = 'True
  x `In` (_ ': xs) =  x `In` xs
  _ `In`  _        = 'False

-- | A heterogeneous, directed, acyclic graph, in which values are evaluated in
-- some @f@ context.
--
-- >>> :set -XFlexibleContexts -XGADTs -XTypeApplications
-- >>> let first  = HNodeF @'[   ] (\ _             -> [25 :: Int])
-- >>> let second = HNodeF @'[Int] (\(xs :$> HNilF) -> map show xs)
--
-- Here, we've defined a node, @second@, that depends on the presence of some
-- 'Int' node. Luckily, @first@ is an @Int@ node!
--
-- >>> :t second $ first $ HEmptyF
-- second $ first $ HEmptyF :: HDagF [] '[String, Int]

data HDagF (f :: Type -> Type) (xs :: [Type]) where
  HEmptyF
    :: HDagF f '[]

  HNodeF
    :: (cs `AreChildrenOf` xs, x `In` xs ~ False)
    => (HListF f cs -> f x) -> HDagF f xs -> HDagF f (x ': xs)

-- | Extract a child node from a DAG by first evaluating its dependencies, and
-- then calculating its own value.
--
-- >>> :set -XGADTs -XTypeApplications
-- >>> extract @Int (HNodeF @'[] (\HNilF -> [2 :: Int]) HEmptyF)
-- [2]

class (x :: Type) `IsAChildOf` (xs :: [Type]) where
  extract :: HDagF f xs -> f x

instance x `IsAChildOf` (x ': xs) where
  extract = evaluate

instance {-# OVERLAPPABLE #-} x `IsAChildOf` xs
    => x `IsAChildOf` (y ': xs) where
  extract (HNodeF _ tail) = extract tail

-- | Extract /multiple/ children from a DAG via repeated use of @IsAChildOf@.
--
-- >>> :set -XFlexibleContexts -XGADTs -XTypeApplications
-- >>> let first  = HNodeF @'[   ] (\ _             ->  [2, 4 :: Int])
-- >>> let second = HNodeF @'[Int] (\(xs :$> HNilF) ->   map show xs )
--
-- >>> let plucked = extracts @'[Int, String] (second $ first $ HEmptyF)
-- >>> :t plucked
-- plucked :: HListF [] '[Int, String]
--
-- >>> import Data.Function ((&))
-- >>> plucked & \(i :$> s :$> HNilF) -> mconcat (map show i ++ s)
-- "2424"

class (cs :: [Type]) `AreChildrenOf` (xs :: [Type]) where
  extracts :: HDagF f xs -> HListF f cs

instance '[] `AreChildrenOf` xs where
  extracts _ = HNilF

instance (c `IsAChildOf` xs, cs `AreChildrenOf` xs)
    => (c ': cs) `AreChildrenOf` xs where
  extracts xs = extract xs :$> extracts xs

-- | Evaluate a DAG computation. We recursively evaluate the children of the
-- node to produce the inputs to its function, and then... well, run that
-- function! Nothing too exciting.
--
-- >>> let first  = HNodeF @'[   ] (\ _             ->  [2, 4 :: Int])
-- >>> let second = HNodeF @'[Int] (\(xs :$> HNilF) ->   map show xs )
--
-- >>> evaluate (second $ first $ HEmptyF)
-- ["2","4"]

evaluate :: HDagF f (x ': xs) -> f x
evaluate (HNodeF f tail) = f (extracts tail)
