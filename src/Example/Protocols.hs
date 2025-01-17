{-# LANGUAGE BlockArguments #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Example.Protocols where

import Clash.Prelude
import qualified Data.Bifunctor as B
import Data.Coerce

import Protocols
import qualified Protocols.Df as Df

-- | Priority arbiter, with FIFO queues on the inputs to buffer values with
-- lower priority
priorityArbiter ::
  forall n dom a depth .
  (KnownNat n, HiddenClockResetEnable dom, 1 <= n, 1 <= depth, NFDataX a) =>
  -- | Depth of the fifo's
  SNat depth ->
  -- | Values you want to arbitrate between, values at the end of vector
  -- have higher priority than the values at the beginning of the vector.
  Vec n (Signal dom (Maybe a)) ->
  -- | Tuple of:
  --
  -- 1. Indicators whether the FIFO's can accept more inputs (True), or not (False)
  -- 2. `Just` the available value with the highest priority; `Nothing` when
  --    there were no available values.
  (Vec n (Signal dom Bool), Signal dom (Maybe a))
priorityArbiter depth@SNat xs =
    (B.bimap coerce (fmap Df.dataToMaybe) .
     toSignals circ .
     B.bimap (fmap (fmap Df.maybeToData)) pure) (xs,Ack True)
 where
  circ :: Circuit (Vec n (Df dom a)) (Df dom a)
  circ =
    -- No fifo queue needed for the highest priority input
    attach @(n-1) (repeatC (Df.fifo depth)) idC |>
    Df.roundrobinCollect Df.Parallel

-- | Add a circuit to the tail of a vector of circuits composed in parallel
--
-- Like (:<), but for Circuit
attach ::
  forall n a b .
  KnownNat n =>
  Circuit (Vec n a) (Vec n b) ->
  Circuit a b ->
  Circuit (Vec (n+1) a) (Vec (n + 1) b)
attach (Circuit f) (Circuit g) = Circuit $ \(fwd,bwd) ->
  let
    (fwdV,fwdE) = splitAt (SNat @n) fwd
    (bwdV,bwdE) = splitAt (SNat @n) bwd
    (bwdY,fwdY) = f (fwdV,bwdV)
    (bwdX,fwdX) = g (head fwdE,head bwdE)
  in
    (bwdY ++ singleton bwdX, fwdY ++ singleton fwdX)
