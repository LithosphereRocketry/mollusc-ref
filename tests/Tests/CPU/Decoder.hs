module Tests.CPU.Decoder where

import Clash.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import CPU.Decoder
import CPU.ISA

decoderTests :: TestTree
decoderTests = testGroup "Decode" [
        testCase "j" $ do 
            let instr = decoder 0x00401234
            itype instr @?= InstrTypeLong InstrJ
            isrca instr @?= PC
            isrcb instr @?= Imm 0x000048d0
            isrcm instr @?= MNone
            idest instr @?= DReg 0
        , testCase "lui" $ do
            let instr = decoder 0x00801234
            itype instr @?= InstrTypeLong InstrLui
            isrca instr @?= AZero
            isrcb instr @?= Imm 0x0048d000
            isrcm instr @?= MNone
            idest instr @?= DReg 0
        , testCase "auipc" $ do
            let instr = decoder 0x00C01234
            itype instr @?= InstrTypeLong InstrAuipc
            isrca instr @?= PC
            isrcb instr @?= Imm 0x0048d000
            isrcm instr @?= MNone
            idest instr @?= DReg 0
        , testCase "add" $ do 
            let instr = decoder 0x01002003
            itype instr @?= InstrTypeNormal InstrAdd
            isrca instr @?= AReg 2
            isrcb instr @?= BReg 3
            isrcm instr @?= MNone
            idest instr @?= DReg 1
        , testCase "add <imm>" $ do 
            let instr = decoder 0x01002523
            itype instr @?= InstrTypeNormal InstrAdd
            isrca instr @?= AReg 2
            isrcb instr @?= Imm 0x123
            isrcm instr @?= MNone
            idest instr @?= DReg 1
        , testCase "add <imm> negative" $ do 
            let instr = decoder 0x010027F0
            itype instr @?= InstrTypeNormal InstrAdd
            isrca instr @?= AReg 2
            isrcb instr @?= Imm 0xFFFFFFF0
            isrcm instr @?= MNone
            idest instr @?= DReg 1
        , testCase "ldp" $ do 
            let instr = decoder 0x02303004
            itype instr @?= InstrTypeLoad (AccessPhysical AccessSizeWord)
            isrca instr @?= AReg 3
            isrcb instr @?= BReg 4
            isrcm instr @?= MNone
            idest instr @?= DReg 2
        , testCase "stp" $ do
            let instr = decoder 0x00372803
            itype instr @?= InstrTypeStore (AccessPhysical AccessSizeWord)
            isrca instr @?= AReg 2
            isrcb instr @?= BReg 3
            isrcm instr @?= MReg 7
            idest instr @?= DMem
    ]