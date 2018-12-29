{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Dagless
Description : An interface for monadically building DAG computations.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

@Dagless@ is a module for building directed, acyclic graph computations using
indexed monads. See the @test@ directory for in-depth examples!
-}
module Dagless
  ( module T

  , In
  , Witness

  , persist
  , using
  ) where

import Control.Monad.Indexed.State (imodify)
import Dagless.Types               as T
import Data.Functor                (($>))
import Data.HDagF                  (AreChildrenOf (..), HDagF (..), In)
import Data.HList                  (HList (..))
import Data.HListF                 (FactorIn (..), FactorOut, HListF (HNilF))
import Data.Kind                   (Constraint, Type)
import Data.Tuple.Morph            (Morph (..))

-- | Witness that something has been added to the DAG. We hide the constructor
-- so users can't create their own witnesses. What this means is that the
-- type errors a user sees /shouldn't/ be from the DAG constraints - they
-- should be type mismatches between the witness and the node function's input.
-- In short, requiring these be passed around should mean some better compiler
-- errors.

data Witness (a :: Type) = Witness -- Hopefully compiler-erased?

-- | We can't pass a tuple to 'using' if we only have one dependency, so this
-- orphan instance means that we can pass a single 'Witness' and still have
-- everything work.

instance Morph '[Witness x] (Witness x) where
  fromTuple  x       = x :> HNil
  toTuple   (x :> _) = x

-- | DAGs are a set of nodes with (directed, acyclic) dependencies on zero or
-- more other nodes in the set. If they have zero nodes, we can think of them
-- as the "leaf" of a tree, and thus their children are effectively constant.
-- 'persist' lets us add a value into the DAG of a 'DaglessT' computation,
-- assuming that no other node exists of the same type. @newtype@ all the
-- things!

persist
  :: (addition `In` nodes ~ False, Monad m)
  => f addition -> DaglessT f m nodes (addition ': nodes) (Witness addition)

persist = fmap (const Witness) . DaglessT . imodify . HNodeF @'[] . const

-- | If your node is /not/ a leaf, and does in fact have requirements on other
-- nodes in the DAG, this function provides a cleaner syntax for accessing
-- them. When nodes are persisted, they produce a 'Witness', which acts as a
-- proof that something exists within the DAG as a result to be calculated.
-- 'using' takes an n-ary (flat) tuple of these witnesses, and a function that
-- produces your node given an equivalent tuple of the calculated values.
-- Again, the examples might be more useful here.

using
  :: ( Morph             proxyTypes                     proxyTuple
     , FactorOut Witness proxyTypes dependencies
     , FactorIn  f                  dependencies fTypes
     , Morph                                     fTypes inputTuple

     , addition `In` nodes ~ 'False
     , dependencies `AreChildrenOf` nodes

     , Monad m
     )
  =>  proxyTuple
  -> (inputTuple -> f addition)
  -> DaglessT f m nodes (addition ': nodes) (Witness addition)

using _ f
  = fmap (const Witness) . DaglessT . imodify
  $ HNodeF (f . toTuple . refactor)
