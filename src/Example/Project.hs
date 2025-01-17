-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans -O0 -fno-omit-interface-pragmas #-}
{-# LANGUAGE BlockArguments, DeriveFunctor, GADTs, DeriveGeneric, FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Example.Project where

import Clash.Explicit.Prelude
import qualified Clash.Signal.Delayed.Bundle as D

import GHC.Generics

myFoldl :: forall b a . (b -> a -> b) -> b -> [a] -> b
myFoldl f = go
  where
    go :: b -> [a] -> b
    go z [] = z
    go z (x:xs) = f (go z xs) x

data MyDataX
  = JustY Int
  | JustZ Bool
  deriving (Generic, NFDataX)

data MySignal a
  = a `FollowedBy` MySignal a
  deriving (Functor, Show)

infixr `FollowedBy`

-- foo :: MySignal Int
-- foo = 1 `FollowedBy` 2 `FollowedBy` (errorX "the end")

myRegister :: a -> MySignal a -> MySignal a
myRegister start inp = start `FollowedBy` inp

myRegister2 :: a -> MySignal Bool -> MySignal a -> MySignal a
myRegister2 rstValue = go rstValue
  where
    go cur (rst `FollowedBy` nextResets) ~(inp `FollowedBy` nextInputs) =
      if rst then
        rstValue `FollowedBy` go rstValue nextResets nextInputs
      else
        cur `FollowedBy` go inp nextResets nextInputs

instance Applicative MySignal where
  pure a = a `FollowedBy` pure a
  (f `FollowedBy` fs) <*> ~(a `FollowedBy` as) =
    f a `FollowedBy` (fs <*> as)

myCounter :: MySignal Bool -> MySignal Int
myCounter rst =
  let cnt = myRegister2 0 rst (liftA2 (+) cnt (pure 1))
   in cnt

resetOnce :: MySignal Bool
resetOnce = True `FollowedBy` True `FollowedBy` pure False

-- Create a domain with the frequency of your input clock. For this example we used
-- 50 MHz.
createDomain vSystem{vName="Dom50", vPeriod=hzToPeriod 50e6}

-- data Configuration n
--   = Configuration
--   { accum1conf :: Unsigned n
--   , accum2conf :: Unsigned n
--   }

data Target
  = ASIC | FPGA

data Configuration where
  Configuration ::
    forall n .
    KnownNat n =>
    {targetInfo :: Target
    ,accum1Conf :: Unsigned n
    ,accum2Conf :: Unsigned n
    } -> Configuration


data MyMaybeX a = NothingX | JustX { fromX :: a }
  deriving Generic

class ShowRep f where
  showRep :: Show (f a) => f a -> String

instance (Constructor c) => ShowRep (M1 i c f) where
  showRep x = show (conName x) <> show x

-- | @topEntity@ is Clash@s equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom50@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.
topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 Word ->
  Signal Dom50 Word
topEntity clk rst ena =
  blockRam clk ena (replicate d1024 0) rd .
  liftA2 toMaybe wen .
  liftA2 (,) wr .
  topLevel (Configuration ASIC (0 :: Unsigned 8) 0) clk rst (andEnable ena (fmap not wen))
 where
  -- ena :: Enable Dom50
  -- ena = enableGen

  rd :: Signal Dom50 Int
  rd = register clk rst ena 0 (rd + 1)

  wen :: Signal Dom50 Bool
  wen = register clk rst ena True (not <$> wen)

  wr :: Signal Dom50 Int
  wr = xyz `hwSeqX` register clk rst ena 1024 (wr - 1)

  toMaybe :: Bool -> a -> Maybe a
  toMaybe False _ = Nothing
  toMaybe _ a = Just a

  xyz :: Signal Dom50 (Vec 1024 (BitVector 32))
  xyz = register clk rst ena undefined xyz

dsignalExample ::
  forall d .
  Clock Dom50 ->
  Vec 8 (DSignal Dom50 d Word) ->
  DSignal Dom50 d Bool ->
  DSignal Dom50 (d + 3) (Word,Bool)
dsignalExample clk vec valid = D.bundle
  ( delayedFold d1 undefined (+) enableGen clk vec
  , delayI False enableGen clk valid
  )

medvedevDSignal ::
  (KnownDomain dom, NFDataX b) =>
  Clock dom -> b -> DSignal dom n a -> (a -> b) -> DSignal dom (n + 1) b
medvedevDSignal clk initialState input0 transition = out
 where
  out = delayN d1 initialState enableGen clk d
  d   = fmap transition input0

type Accumulator dom n =
  (KnownDomain dom, KnownNat n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom (Unsigned n) ->
  Signal dom (Unsigned n)

topLevel ::
  Configuration ->
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 Word ->
  Signal Dom50 Word
topLevel (Configuration _ c1 c2) clk rst ena inp =
  setName @"foo" (accum c1) clk rst ena
  (setName @"bar" (accum c2) clk rst ena inp)

-- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN topEntity
  (Synthesize
    { t_name = "accumXYZ"
    , t_inputs = [ PortName "CONF"
                 , PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}

-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}

-- | A simple accumulator that works on unsigned numbers of any size.
-- It has hidden clock, reset, and enable signals.
accum ::
  forall dom n .
  (KnownDomain dom, KnownNat n) =>
  Unsigned n ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Word ->
  Signal dom Word
accum start clk rst ena inp = setName @"state" mealy clk rst ena accumT start inp
 where
  accumT s i = (s + (fromIntegral i), fromIntegral s)
{-# OPAQUE accum #-}
