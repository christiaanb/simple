{-# LANGUAGE BlockArguments #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Example.ProtocolsTutorial where

import Clash.Prelude

import qualified Data.Bifunctor as B

import Protocols
import qualified Protocols.Df as Df

-- | This circuit just passes along data
idDf ::
  -- |
  -- 1. Data received from upstream
  -- 2. Back-pressure from downsteam
  (Signal dom (Df.Data a), Signal dom Ack) ->
  -- |
  -- 1. Back-pressure to upstream
  -- 2. Data send to downstream
  (Signal dom Ack, Signal dom (Df.Data a))
idDf (d,ack) = (ack,d)

-- | A protocol describes the in- and outputs of one side of a 'Circuit'.
-- class Protocol a where
--   -- | Sender to receiver type family. See 'Circuit' for an explanation on the
--   -- existence of 'Fwd'.
--   type Fwd (a :: Type)
--
--   -- | Receiver to sender type family. See 'Circuit' for an explanation on the
--   -- existence of 'Bwd'.
--   type Bwd (a :: Type)

-- @
--             Circuit a b
--
--            +-----------+
--     Fwd a  |           |  Fwd b
--   +------->+           +-------->
--            |           |
--            |           |
--     Bwd a  |           |  Bwd b
--   <--------+           +<-------+
--            |           |
--            +-----------+
-- @
-- newtype Circuit a b
--   = Circuit ((Fwd a, Bwd b) -> (Bwd a, Fwd b))

idDfCircuit :: Circuit (Df.Df dom a) (Df.Df dom a)
idDfCircuit = Circuit idDf

-- | One place FIFO, delay of one (no cut-through behavior)
onePlaceFifo ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  -- |
  -- 1. Data received from upstream
  -- 2. Back-pressure from downsteam
  (Signal dom (Df.Data a), Signal dom Ack) ->
  -- |
  -- 1. Back-pressure to upstream
  -- 2. Data send to downstream
  (Signal dom Ack, Signal dom (Df.Data a))
onePlaceFifo = mealyB onePlaceFifoT Df.NoData
  where
    onePlaceFifoT Df.NoData (dataIn,_ackIn) = (newState,(ackOut,dataOut))
      where
        newState = dataIn
        ackOut   = Ack True
        dataOut  = Df.NoData

    onePlaceFifoT (Df.Data a) (dataIn,ackIn) = (newState,(ackOut,dataOut))
      where
        newState
          | Ack True <- ackIn = dataIn
          | otherwise         = Df.Data a

        ackOut = ackIn

        dataOut = Df.Data a

-- | One place FIFO, delay of one (no cut-through behavior)
onePlaceFifoCircuit ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  Circuit (Df.Df dom a) (Df.Df dom a)
onePlaceFifoCircuit = Circuit onePlaceFifo

-- | Three one-place fifo's chained together
fifoChain ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  -- |
  -- 1. Data received from upstream
  -- 2. Back-pressure from downsteam
  (Signal dom (Df.Data a), Signal dom Ack) ->
  -- |
  -- 1. Back-pressure to upstream
  -- 2. Data send to downstream
  (Signal dom Ack, Signal dom (Df.Data a))
fifoChain (dIn,ackIn) =
  let
    (ackOut0,dOut0) = onePlaceFifo (dIn, ackOut1)
    (ackOut1,dout1) = onePlaceFifo (dOut0, ackOut2)
    (ackOut2,dout2) = onePlaceFifo (dout1, ackIn)
  in
    (ackOut0,dout2)

fifoChainCircuit ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  Circuit (Df.Df dom a) (Df.Df dom a)
fifoChainCircuit =
  onePlaceFifoCircuit |>
  onePlaceFifoCircuit |>
  onePlaceFifoCircuit

fifoChainCircuitNotation ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  Circuit (Df.Df dom a) (Df.Df dom a)
fifoChainCircuitNotation = circuit $ \df -> do
  df0 <- onePlaceFifoCircuit -< df
  df1 <- onePlaceFifoCircuit -< df0
  df2 <- onePlaceFifoCircuit -< df1
  idDfCircuit -< df2

-- | Make a "standard" signal adhere to the Df protocol, drops values when there is back-pressure
-- delay of one cycle
signalToDf ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  -- |
  -- 1. Data received from upstream
  -- 2. Back-pressure from downsteam
  (Signal dom a, Signal dom Ack) ->
  -- |
  -- 1. Back-pressure to upstream
  -- 2. Data send to downstream
  (Signal dom (), Signal dom (Df.Data a))
signalToDf = mealyB signalToDfT Df.NoData
  where
    signalToDfT s (dIn,ackIn) = (newState,((),s))
      where
        newState
          | Df.Data _ <- s
          , Ack True <- ackIn
          = Df.Data dIn
          | otherwise
          = s

-- | Make a signal adhere to the Df protocol, drops values when there is back-pressure
-- delay of one cycle
signalToDfCircuit ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  Circuit (CSignal dom a) (Df dom a)
signalToDfCircuit = Circuit signalToDf


-- | Make a signal adhere to the Df protocol, drops values when there is back-pressure
-- delay of one cycle
--
-- Emit the number of dropped values
signalToDfDrops ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  -- |
  -- 1. Data received from upstream
  -- 2. Back-pressure from downsteam
  --    a. From the Df protocol
  --    b. '()' from "Signal protocol"
  (Signal dom a, (Signal dom Ack, Signal dom ())) ->
  -- |
  -- 1. Back-pressure to upstream
  -- 2. Data send to downstream
  (Signal dom (), (Signal dom (Df.Data a), Signal dom Word))
signalToDfDrops = B.second unbundle . mealyB signalToDfCountT (Df.NoData,0) . B.second bundle
  where
    signalToDfCountT (s,cnt) (dIn,(ackIn,_)) = ((newState,newCnt),((),(s,cnt)))
      where
        newState
          | Df.Data _ <- s
          , Ack True <- ackIn
          = Df.Data dIn
          | otherwise
          = s

        newCnt
          | Df.Data _ <- s
          , Ack False <- ackIn
          = cnt + 1
          | otherwise
          = cnt

signalToDfDropCircuit ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  Circuit (CSignal dom a) (Df dom a, CSignal dom Word)
signalToDfDropCircuit = Circuit signalToDfDrops


-- idC :: forall a. Circuit a a
-- idC = Circuit swap

-- | Make a signal adhere to the Df protocol, drops values when there is back-pressure.
-- Add a one-place fifo in the chain.
--
-- delay of two cycles
--
-- Emit the number of dropped values
signalToDfFifo ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  Circuit (CSignal dom a) (Df.Df dom a, CSignal dom Word)
signalToDfFifo = circuit $ \s -> do
  (df0,cnt) <- signalToDfDropCircuit -< s
  df1 <- onePlaceFifoCircuit -< df0
  idC -< (df1,cnt)

-- | Make a signal adhere to the Df protocol, drops values when there is back-pressure.
-- Add a one-place fifo in the chain.
--
-- delay of two cycles
--
-- Emit whether a value is dropped
signalToDfFifo2 ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  Circuit (CSignal dom a) (Df.Df dom a, CSignal dom Bool)
signalToDfFifo2 = circuit $ \s -> do
  (df0,Fwd cnt) <- signalToDfDropCircuit -< s
  df1 <- onePlaceFifoCircuit -< df0
  idC -< (df1,Fwd (fmap (>0) cnt))


usingVectors ::
 (HiddenClockResetEnable dom, NFDataX a) =>
 Circuit (CSignal dom a) (Df.Df dom a,CSignal dom Word)
usingVectors = circuit $ \s -> do
  (df,cnt) <- signalToDfDropCircuit -< s
  [df1,df2,df3] <- Df.fanout -< df
  df4 <- Df.roundrobinCollect Df.Parallel -< [df1,df2,df3]
  idC -< (df4,cnt)
