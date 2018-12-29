{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Test.Energy where

import Dagless                     (DaglessT', Witness, compute', persist, using)
import Data.HDagF                  (In)
import Data.HList                  (HList (..), PluckedFrom (..))
import Language.Haskell.DoNotation ((>>=), pure, return)
import Prelude                     hiding ((>>=), (>>), pure, return)

-- We're going to build a DAG to calculate the energy output of an object at a
-- given mass accelerating at a given rate over a given displacement. I'm
-- afraid my understanding of mechanics means that we're going to have to
-- assume it's a perfectly spherical item in a vacuum, etc etc.  First, we're
-- going to need a bunch of types:

newtype Acceleration = Acceleration { getA :: Double }
newtype Displacement = Displacement { getD :: Double }
newtype Energy       = Energy       { getE :: Double } deriving Show
newtype Force        = Force        { getF :: Double }
newtype Mass         = Mass         { getM :: Double }

-- To make the example neater, let's be explicit about the types that we're
-- expecting as input:

type Input = '[ Acceleration, Mass, Displacement ]

-- Now everything's in place, let's look at a little computation. Note that, as
-- we don't care about our context, this computation is polymorphic in its
-- monad.

main :: Monad m => HList Input -> m Energy
main collection = compute' do
    mass         <- fetch @Mass
    acceleration <- fetch @Acceleration

    force <- using (mass, acceleration) $ \(m, a) -> do
      Mass         m' <- m
      Acceleration a' <- a

      pure (Force (m' * a'))

    displacement <- fetch @Displacement

    using (force, displacement) $ \(f, d) -> do
      Force        f' <- f
      Displacement d' <- d

      pure (Energy (f' * d'))

  where
    -- We're also going to need a function that allows us to take something
    -- from this collection and add it to the DAG. Here, we get some help from
    -- the 'persist' function.

    fetch
      :: forall x f m xs
       . (Monad m, x `In` xs ~ False, x `PluckedFrom` Input)
      => DaglessT' m xs (x ': xs) (Witness x)

    fetch
      = persist (pure (pluck @x collection))
