module CPU.Regfile where

import qualified Prelude as P
import Clash.Prelude

import CPU.Forward

withZeroReg :: (Enum addr, HiddenClockResetEnable dom, NFDataX addr, Num addr, Eq addr, NFDataX a, Num a)
    => (SNat n -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
    -> (SNat n -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
withZeroReg backingMemory n addr write = mux (addr .==. pure 0)
    (pure 0)
    (backingMemory n addr write)

withForwarding :: (Enum addr, HiddenClockResetEnable dom, NFDataX addr, Num addr, Eq addr, NFDataX a, Num a)
    => (SNat n -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
    -> (SNat n -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
withForwarding backingMemory n addr write =
    let backed_value = backingMemory n addr write
        (_, result) = unbundle $ fwdMaybe (bundle (addr, backed_value)) write
    in result

asRegFile :: (Enum addr, HiddenClockResetEnable dom, NFDataX addr, NFDataX a)
    => (SNat n -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a)
    -> SNat n -- number of registers to create
    -> Signal dom (Maybe (addr, a)) -- incoming write - Just (addr, data) if writing, Nothing otherwise
    -> [Signal dom addr] -- List of addresses to read
    -> [Signal dom a] -- List of values read
asRegFile backingMemory n write = P.map (\addr -> backingMemory n addr write)

molluscRegFile :: (Enum addr, HiddenClockResetEnable dom, NFDataX addr, Num addr, Eq addr, NFDataX a, Num a)
    => Signal dom (Maybe (addr, a))
    -> [Signal dom addr]
    -> [Signal dom a]
molluscRegFile = asRegFile (withZeroReg asyncRam) (SNat @16)
