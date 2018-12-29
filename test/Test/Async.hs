{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Test.Async where

import Control.Concurrent          (threadDelay)
import Control.Concurrent.Async    (concurrently)
import Control.Monad.Indexed.Trans (ilift)
import Dagless                     (DaglessT', In, Witness, persist, root', using)
import Data.Coerce                 (coerce, Coercible)
import Language.Haskell.DoNotation ((>>=), (>>), pure, return)
import Prelude                     hiding ((>>=), (>>), pure, return)
import System.Random               (randomIO)

-- In this example, we'll imagine some networked game between four people
-- divided into two teams. The goal here is to add the scores per team, and
-- then see which team has the most points. We'll throw in a little bit of
-- thread delay just to make it more interesting.

-- We're going to need some new types for our DAG. First, players' individual
-- scores.

newtype Player1Total = Player1Total Int
newtype Player2Total = Player2Total Int
newtype Player3Total = Player3Total Int
newtype Player4Total = Player4Total Int

-- Secondly, the team's scores...

newtype TeamATotal = TeamATotal Int
newtype TeamBTotal = TeamBTotal Int

-- Finally, the result of the match!

data Result = TeamA | TeamB | Draw
  deriving Eq

main :: Int -> Int -> Int -> Int -> IO Result
main x1 x2 x3 x4 = root' do
  -- We register all 4 as things we need. I've called it 'mimic' because it's
  -- just mimicking network delay, and not actually calculating anything.

  p1 <- mimic @Player1Total x1
  p2 <- mimic @Player2Total x2
  p3 <- mimic @Player3Total x3
  p4 <- mimic @Player4Total x4

  -- Get the A-team scores...
  teamA <- using (p1, p2) \(p1, p2) -> do
    (Player1Total p1', Player2Total p2') <- concurrently p1 p2
    pure (TeamATotal (p1' + p2'))

  -- Get the B-team scores...
  teamB <- using (p3, p4) \(p3, p4) -> do
    (Player3Total p3', Player4Total p4') <- concurrently p3 p4
    pure (TeamBTotal (p3' + p4'))

  -- Finally, we can calculate the winner!
  using (teamA, teamB) \(tA, tB) -> do
    (TeamATotal tA', TeamBTotal tB') <- concurrently tA tB

    pure case compare tA' tB' of
      GT -> TeamA
      LT -> TeamB
      EQ -> Draw

  where

    -- Mimic some server delay. We'll sleep for a random number of
    -- milliseconds, then yield the original value.

    mimic
      :: forall x xs
       . (Coercible x Int, x `In` xs ~ False)
      => Int
      -> DaglessT' IO xs (x ': xs) (Witness x)

    mimic x = do
      sleep <- ilift randomIO
      ilift (threadDelay (sleep `mod` 50))

      persist (pure (coerce x))
