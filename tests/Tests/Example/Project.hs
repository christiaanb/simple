module Tests.Example.Project where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import qualified Clash.Explicit.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Import the module containing the @accum@ function
import Example.Project (accum)

testBenchAccum ::
  -- | Duration
  Int ->
  -- | Input samples
  [Word] ->
  -- | Results outputs
  [Word]
testBenchAccum duration inps = realOuts
  where
    realOuts = (drop 1 .
                C.sampleN (duration+1) .
                (accum @C.System @64 0 C.clockGen C.resetGen C.enableGen) .
                C.fromList .
                dup1) inps

    dup1 [] = C.errorX "no inputs"
    dup1 (x:xs) = x:x:xs

-- Define a HUnit unit test to test the @accum@ function
case_unit :: Assertion
case_unit =
  assertBool "accum [1,2,3,4,5] == [0,1,3,6,10]"
    (testBenchAccum 5 [1..5] == [0,1,3,6,10])

-- Define a Hedgehog property to test the @accum@ function
prop_accum :: H.Property
prop_accum = H.property $ do

  -- Simulate for a random duration between 1 and 100 cycles
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  -- Generate a list of random unsigned numbers.
  inp <- H.forAll
    (Gen.list (Range.singleton simDuration)
    (Gen.word Range.linearBounded))
  let

    -- Calculate the expected output. The first cycle is the initial value, and
    -- the result of the final input value does not appear because the
    -- accumulator has 1 cycle latency.
    expected = init (scanl (+) 0 inp)

    -- Simulate the @accum@ function for the pre-existing @System@ domain
    -- and 8 bit unsigned numbers.
    --
    -- The (hidden) reset input of @accum@ will be asserted in the first cycle;
    -- during this cycle it will emit its initial value and the input is
    -- ignored. So we need to present a dummy input value.
    simOut = testBenchAccum simDuration inp

  -- Check that the simulated output matches the expected output
  simOut H.=== expected

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
