module Tests.CPU.Regfile where

import qualified Prelude as P
import Clash.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import CPU.Regfile

regfileTests :: TestTree
regfileTests = testGroup "Regfile" [
        testCase "writeRead" $ let
            out = sampleN @System 3 (
                    P.head $ molluscRegFile (pure $ Just (3 :: Unsigned 4, 12345 :: Unsigned 32)) [3]
                )
        in out P.!! 2 @?= 12345,
        testCase "writeReadZero" $ let
            out = sampleN @System 3 (
                    P.head $ molluscRegFile (pure $ Just (0 :: Unsigned 4, 12345 :: Unsigned 32)) [0]
                )
        in out P.!! 2 @?= 0
        -- Forwarding is handled outside the register file for now
        -- so these tests aren't relevant
        -- testCase "writeReadForward" $ let
        --     out = sampleN @System 3 (
        --             P.head $ molluscRegFile (fromList [
        --                 Just (3 :: Unsigned 4, 12345 :: Unsigned 32),
        --                 Just (3 :: Unsigned 4, 12345 :: Unsigned 32),
        --                 Just (3 :: Unsigned 4, 54321 :: Unsigned 32)
        --             ]) [3]
        --         )
        -- in out P.!! 2 @?= 54321,
        -- testCase "writeReadNoForward" $ let
        --     out = sampleN @System 3 (
        --             P.head $ molluscRegFile (fromList [
        --                 Just (3 :: Unsigned 4, 12345 :: Unsigned 32),
        --                 Just (3 :: Unsigned 4, 12345 :: Unsigned 32),
        --                 Just (6 :: Unsigned 4, 54321 :: Unsigned 32)
        --             ]) [3]
        --         )
        -- in out P.!! 2 @?= 12345,
        -- testCase "writeReadZeroNoForward" $ let
        --     out = sampleN @System 3 (
        --             P.head $ molluscRegFile (fromList [
        --                 Just (3 :: Unsigned 4, 12345 :: Unsigned 32),
        --                 Just (3 :: Unsigned 4, 12345 :: Unsigned 32),
        --                 Just (0 :: Unsigned 4, 54321 :: Unsigned 32)
        --             ]) [0]
        --         )
        -- in out P.!! 2 @?= 0
    ]

main :: IO ()
main = defaultMain regfileTests
