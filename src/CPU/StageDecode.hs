module CPU.StageDecode where

import Clash.Prelude
import Protocols
-- import qualified Protocols.Df as Df

import CPU.Common
import CPU.ISA (InstrType)
import CPU.Decoder (decoder)
import CPU.Regfile (molluscRegFile)

import qualified CPU.StageFetch as Fetch

data DecodeFields = DecodeFields {
    fpc :: Address,
    fitype :: InstrType,
    frega :: (RegAddr, IntWord),
    fregb :: (RegAddr, IntWord),
    fregm :: (RegAddr, IntWord),
    fregp :: (PredAddr, Predicate)
}

stageDecode :: Circuit (Df dom Fetch.FetchFields, Signal dom Bool) (Df dom DecodeFields)
stageDecode = Circuit stageDecodeImpl

stageDecodeImpl ::
        (Signal dom (Maybe Fetch.FetchFields), Signal dom Ack)
     -> (Signal dom Ack, Signal dom (Maybe DecodeFields))
stageDecodeImpl (fetch, d_ack) = unbundle (fmap stageDecodeT (bundle (fetch, d_ack)))

fetchedRegT :: (Eq addr)
     => addr
     -> Maybe (addr, val)
     -> (Maybe (Maybe (addr, val)), Maybe (addr, val))
     -> Maybe (addr, val)
-- fetchedRegT sentinel current (incoming, forwarded) = next
-- if we are loading a new instruction but no new instruction is ready, we become Nothing
fetchedRegT _ _ (Just Nothing, _) = Nothing
-- if we contain nothing, and no instruction is incoming, we never forward
fetchedRegT _ Nothing (Nothing, _) = Nothing
-- if we have an instruction, and no new instruction is incoming, and no forward value is provided, don't forward
fetchedRegT _ (Just (addr, val)) (Nothing, Nothing) = Just (addr, val)
-- if there's an incoming instruction, and no forward, take it
fetchedRegT _ _ (Just instr, Nothing) = instr
-- if there's an incoming instruction, and a forward, compare and choose
fetchedRegT sentinel _ (Just (Just (addr, val)), Just (fwd_addr, fwd_val)) =
    if addr /= sentinel && addr == fwd_addr then Just (fwd_addr, fwd_val) else Just (addr, val)
-- if there's no incoming instruction, but we have contents, and there's a forward, compare and choose
fetchedRegT sentinel (Just (addr, val)) (Nothing, Just (fwd_addr, fwd_val)) =
    if addr /= sentinel && addr == fwd_addr then Just (fwd_addr, fwd_val) else Just (addr, val)

fetchedReg :: (HiddenClockResetEnable dom, Num addr, Eq addr, NFDataX addr, NFDataX val)
     => Signal dom (Maybe (Maybe (addr, val)), Maybe (addr, val))
     -> Signal dom (Maybe (addr, val))
fetchedReg = moore (fetchedRegT 0) id Nothing

