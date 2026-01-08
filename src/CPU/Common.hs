module CPU.Common where

import Clash.Prelude

type RegAddr = Unsigned 4
type IntWord = Unsigned 32
type Address = IntWord

type PredAddr = (Bool, Unsigned 3)
type Predicate = Bool

data RegWrite = Register RegAddr | Predicate PredAddr
