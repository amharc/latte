{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Backend.Peephole(opt) where

import Control.Lens
import Data.Data
import Data.Data.Lens
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Language.Latte.Backend.Asm

opt :: Seq.Seq Instruction -> Seq.Seq Instruction
opt = forward . fst . foldr backward (Seq.empty, Nothing)

forward :: Seq.Seq Instruction -> Seq.Seq Instruction
forward seq = case Seq.viewl seq of
    Seq.EmptyL -> Seq.empty
    Mov _ op op' Seq.:< instrs | op == op' -> forward instrs
    i Seq.:< instrs -> i Seq.<| forward instrs

type Acc = (Seq.Seq Instruction, Maybe (Set.Set Register))

backward :: Instruction -> Acc -> Acc
backward AnnotateLiveStop (instrs, _) = (AnnotateLiveStop Seq.<| instrs, Nothing)
backward i@(AnnotateLiveStart live) (instrs, _) = (i Seq.<| instrs, Just live)
backward i (instrs, Nothing) = (i Seq.<| instrs, Nothing)
backward instr old@(instrs, live) = case instr of
    AnnotateLiveStop -> (insertedInstrs, Nothing)
    AnnotateLiveStart live -> (insertedInstrs, Just live)
    _ | Just l <- live
      , not removable
      -> (insertedInstrs, Just $ uses instr `Set.union` (l `Set.difference` kills instr))
      | otherwise
      -> old
  where
    insertedInstrs = instr Seq.<| instrs

    removable :: Bool
    removable = case live of
        Nothing -> False
        Just l -> not (protected instr) && Set.null (l `Set.intersection` kills instr)

    uses :: Instruction -> Set.Set Register
    uses instr = uses' instr `Set.union` (allRegs (instr ^.. template :: [Memory]))

    uses' :: Instruction -> Set.Set Register
    uses' (Mov _ (OpRegister src) _) = allRegs src
    uses' (Lea _ (OpRegister src) _) = allRegs src
    uses' (Idiv _ op) = allRegs (op, RAX, RDX)
    uses' (Set _ _) = allRegs [RFLAGS]
    uses' (Call op) = allRegs (op, RDI, RSI, RDX, RCX, R8, R9)
    uses' (Movsbq src _) = allRegs src
    uses' Cqto = allRegs [RAX]
    uses' (Push _ (OpRegister val)) = allRegs [RSP, val]
    uses' (Push _ _) = allRegs [RSP]
    uses' (Pop _ (OpRegister val)) = allRegs [RSP, val]
    uses' (Pop _ _) = allRegs [RSP]
    uses' (JumpCond _ _) = allRegs [RFLAGS]
    uses' Leave = allRegs [RSP, RBP]
    uses' Ret = allRegs [RAX]
    uses' i = allRegs i

    kills :: Instruction -> Set.Set Register
    kills (Mov _ _ (OpRegister dst)) = allRegs [dst]
    kills (Add _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Add _ _ _) = allRegs [RFLAGS]
    kills (Sub _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Sub _ _ _) = allRegs [RFLAGS]
    kills (Imul _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Imul _ _ _) = allRegs [RFLAGS]
    kills (Lea _ _ (OpRegister dst)) = allRegs [dst]
    kills (And _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (And _ _ _) = allRegs [RFLAGS]
    kills (Or _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Or _ _ _) = allRegs [RFLAGS]
    kills (Xor _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Xor _ _ _) = allRegs [RFLAGS]
    kills (Sal _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Sal _ _ _) = allRegs [RFLAGS]
    kills (Sar _ _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Sar _ _ _) = allRegs [RFLAGS]
    kills (Neg _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Neg _ _) = allRegs [RFLAGS]
    kills (Idiv _ _) = allRegs [RFLAGS, RAX, RDX]
    kills (Cmp _ _ _) = allRegs [RFLAGS]
    kills (Movsbq _ (OpRegister dst)) = allRegs [RFLAGS, dst]
    kills (Movsbq _ _) = allRegs [RFLAGS]
    kills Cqto = allRegs [RDX]
    kills (Inc _ (OpRegister dst)) = allRegs [dst]
    kills (Dec _ (OpRegister dst)) = allRegs [dst]
    kills (Push _ _) = allRegs [RSP]
    kills (Pop _ (OpRegister dst)) = allRegs [RSP, dst]
    kills (Pop _ _) = allRegs [RSP]
    kills (Call _) = allRegs [RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11]
    kills Leave = allRegs [RBP, RSP]
    kills _ = Set.empty

    protected :: Instruction -> Bool
    protected (Mov _ _ (OpMemory _)) = True
    protected (Add _ _ (OpMemory _)) = True
    protected (Sub _ _ (OpMemory _)) = True
    protected (Imul _ _ (OpMemory _)) = True
    protected (Lea _ _ (OpMemory _)) = True
    protected (And _ _ (OpMemory _)) = True
    protected (Or _ _ (OpMemory _)) = True
    protected (Xor _ _ (OpMemory _)) = True
    protected (Sal _ _ (OpMemory _)) = True
    protected (Sar _ _ (OpMemory _)) = True
    protected (Neg _ (OpMemory _)) = True
    protected (Push _ _) = True
    protected (Pop _ _) = True
    protected (Jump _) = True
    protected (JumpCond _ _) = True
    protected (Label _) = True
    protected (GlobalFunc _) = True
    protected (Type _ _) = True
    protected (String _) = True
    protected (Quad _) = True
    protected (QuadInt _) = True
    protected (Align _) = True
    protected (Call _) = True
    protected (AnnotateLiveStart _) = True
    protected AnnotateLiveStop = True
    protected Leave = True
    protected Ret = True
    protected _ = False

    allRegs :: Data a => a -> Set.Set Register
    allRegs x = Set.fromList $ x ^.. template
