import Prelude

import Test.Tasty

import qualified Tests.CPU.Regfile
import qualified Tests.CPU.Decoder
import qualified Tests.CPU.StageDecode

main :: IO ()
main = defaultMain $ testGroup "."
  [
    Tests.CPU.Regfile.regfileTests,
    Tests.CPU.Decoder.decoderTests,
    Tests.CPU.StageDecode.stageDecodeTests
  ]
