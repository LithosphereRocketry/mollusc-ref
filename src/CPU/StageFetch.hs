module CPU.StageFetch where

import Clash.Prelude

data FetchFields = FetchFields {
    fpc :: Unsigned 32,
    finstr :: Unsigned 32
}