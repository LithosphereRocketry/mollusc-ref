module CPU.Forward where

import Clash.Prelude
import Data.Maybe (fromMaybe)

-- TODO - does it make sense for the forwarder here to block the 0 register, or
-- should that be external in some way?

-- Forwards non-zero-register incoming data in place of original data when
-- register numbers match. Does not forward when incoming data is Nothing.
fwdMaybe :: (HiddenClockResetEnable dom, Eq addr, Num addr, NFDataX value)
    => Signal dom (addr, value) -- Original source register and value
    -> Signal dom (Maybe (addr, value)) -- Incoming source register and value
    -> Signal dom (Bool, value) -- True if forward is used; resultant value
fwdMaybe original maybe_incoming = let
        (org_addr, org_val) = unbundle original
        (inc_addr, inc_val) = unbundle $ fromMaybe
                <$> bundle (pure 0, deepErrorX "Forwarded from nonexistent value")
                <*> maybe_incoming
        use_incoming = inc_addr ./=. pure 0 .&&. inc_addr .==. org_addr
    in bundle (use_incoming, mux use_incoming inc_val org_val)

-- Forwards non-zero-register incoming data in place of original data when
-- register numbers match.
forwarder :: (HiddenClockResetEnable dom, Eq addr, Num addr, NFDataX value)
    => Signal dom (addr, value) -- Original source register and value
    -> Signal dom (addr, value) -- Incoming source register and value
    -> Signal dom (Bool, value) -- True if forward is used; resultant value
forwarder org inc = fwdMaybe org (Just <$> inc)
