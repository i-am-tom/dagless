{-# LANGUAGE BlockArguments #-}
module Main where

import           Data.HList       (HList (..))
import           System.IO.Unsafe (unsafePerformIO)
import           Test.DocTest
import qualified Test.Async       as Async
import qualified Test.Energy      as Energy
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = do
  -- The documentation in the src/ folder contains many of the tests. The
  -- 'doctest' program parses the source files for examples, and runs them!

  doctest ["src", "test"]

  -- We also have some examples in this @test/@ directory, which we test with
  -- Hspec. Possibly not /much/ use in property-testing here, but it's
  -- certainly better than nothing.

  hspec do

    -- The Energy example demonstrates a fully context-polymorphic DAG, meaning
    -- that all the computations are totally pure and without side-effects. The
    -- calculation is fairly straightforward, but introduces a couple of
    -- dependencies: we calculate @Force@ using @Mass@ and @Acceleration@, then
    -- we calculate @Energy@ using @Force@ and @Displacement@. If everything
    -- works out, this is just a long-winded way of multiplying the original
    -- three values!

    describe "Energy" do
      it "Calculates the correct energy value" $ property \a m d -> do
        value <- Energy.main $ Energy.Acceleration a
                            :> Energy.Mass m
                            :> Energy.Displacement d
                            :> HNil

        Energy.getE value `shouldBe` (a * m * d)

    -- The Async example is a little more interesting. We explicitly encode the
    -- dependencies in our asynchronous computations, making maximal use of
    -- parallelism by default. We've used 'concurrently' explicitly in the
    -- 'using' functions, but we needn't: we could just as easily hide this
    -- behaviour behind a custom 'using' function and expose a DSL in which
    -- dependencies were automatically evaluated in parallel!

    describe "Async" do
      it "Properly schedules asynchronous actions" $ property \a b c d ->
        unsafePerformIO (Async.main a b c d)
          == case compare (a + b) (c + d) of
               GT -> Async.TeamA
               EQ -> Async.Draw
               LT -> Async.TeamB
