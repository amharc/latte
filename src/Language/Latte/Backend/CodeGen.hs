{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-type-defaults #-}
module Language.Latte.Backend.CodeGen where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Coerce
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Language.Latte.Backend.Asm as Asm
import Language.Latte.Middleend.IR
import Language.Latte.Backend.RegAlloc

default (Int)

data EmitterState = EmitterState
    { _esInstructions :: Seq.Seq Asm.Instruction
    , _esRegisterState :: Map.Map Asm.Register RegisterState
    , _esPreferredLocations :: Map.Map Name Asm.RegisterOrSpill
    , _esLockedRegisters :: Set.Set Asm.Register
    }

data RegisterState = RSReg | RSMem | RSRegMem

makeLenses ''EmitterState
makePrisms ''RegisterState

-- |Emits a function
--
-- Precondition: valid SSA, mem2reg applied
emitFunction :: MonadIO m => Ident -> Block -> m (Seq.Seq Asm.Instruction)
emitFunction functionName entryBlock = do
    blocks <- reachableBlocks entryBlock
    preferredLocations <- fmap preprocessLocations <$> allocateRegisters blocks
    arguments <- argumentsOfEntryBlock entryBlock
    evalStateT (do
        forM_ blocks translateBlock
        use $ esInstructions
        )
        EmitterState
            { _esInstructions = Seq.empty
            , _esRegisterState = Map.fromList [(reg, RSReg) | reg <- registerOrder]
            , _esPreferredLocations = preferredLocations
            , _esLockedRegisters = Set.empty
            }
  where
    preprocessLocations i
        | Just reg <- registerOrder ^? ix i = Asm.RSRegister reg
        | otherwise = Asm.RSSpill (i - length registerOrder)

argumentsOfEntryBlock :: MonadIO m => Block -> m Int
argumentsOfEntryBlock entryBlock = do
    body <- liftIO . readIORef $ entryBlock ^. blockBody
    pure . fromMaybe 0 $ maximumOf (traverse . instrPayload . _ILoad . loadFrom . _MemoryArgument) body

emit :: MonadState EmitterState m => Asm.Instruction -> m ()
emit instr = esInstructions %= flip snoc instr

translateBlock :: (MonadIO m, MonadState EmitterState m) => Block -> m ()
translateBlock block = do
    emit . Asm.Label $ blockIdent block

    body <- liftIO . readIORef $ block ^. blockBody
    forM_ body translateInstr

    liftIO (readIORef $ block ^. blockEnd) >>= \case
        BlockEndNone -> pure ()
        BlockEndReturn arg -> do
            lockInGivenRegister arg Asm.RAX
            emit Asm.Leave
            emit Asm.Ret
        BlockEndReturnVoid -> do
            emit Asm.Leave
            emit Asm.Ret
        BlockEndBranch target -> do
            forM_ registerOrder restoreRegister
            regs <- use esRegisterState

            emit $ Asm.Jump (phiBlockIdent block target)
            linkTo target regs

            esRegisterState .= regs
        BlockEndBranchCond cond ifTrue ifFalse -> do
            op <- getAsmOperand cond
            emit $ Asm.Test op (Asm.OpImmediate 1)
            emit $ Asm.JumpCond Asm.FlagEqual (phiBlockIdent block ifFalse)
            emit $ Asm.Jump (phiBlockIdent block ifTrue)

            forM_ registerOrder restoreRegister
            regs <- use esRegisterState

            linkTo ifTrue regs
            esRegisterState .= regs

            linkTo ifFalse regs
            esRegisterState .= regs

    unlockRegisters
  where
    linkTo target regs = pure ()
    {-
        emit . AsmLabel $ phiBlockIdent block target
        phis <- liftIO . readIORef $ block ^. blockPhi
        forM_ phis $ \phi ->
            forM_ (phi ^. phiBranches) $ \branch ->        
        emit . AsmJump $ blockIdent target -}

translateInstr :: MonadState EmitterState m => Instruction -> m ()
translateInstr instr = case (instr ^. instrResult, instr ^. instrPayload) of
    (Nothing, Call _ _) -> undefined
    (Nothing, IIntristic _) -> undefined
    (Nothing, Store to size (OperandInt i)) -> do
        mem <- translateMemory to
        emit $ Asm.Mov (sizeToMult size) (Asm.OpImmediate i) (Asm.OpMemory mem)
    (Nothing, Store to size (OperandSize sz)) -> do
        mem <- translateMemory to
        emit $ Asm.Mov (sizeToMult size) (Asm.OpImmediate $ sizeToInt sz) (Asm.OpMemory mem)
    (Nothing, Store to size value) -> do
        reg <- lockInRegister value
        mem <- translateMemory to
        emit $ Asm.Mov (sizeToMult size) (Asm.OpRegister reg) (Asm.OpMemory mem)
        unlockRegisters
    (Nothing, _) -> pure ()

    (Just name, Load from size) -> do
        reg <- lockInRegister name
        mem <- translateMemory from 
        emit $ Asm.Mov (sizeToMult size) (Asm.OpMemory mem) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, BinOp lhs BinOpPlus rhs) -> simpleBinOp name lhs rhs Asm.Add
    (Just name, BinOp lhs BinOpMinus rhs) -> simpleBinOp name lhs rhs Asm.Sub
    (Just name, BinOp lhs BinOpTimes rhs) -> simpleBinOp name lhs rhs Asm.Imul
    (Just name, BinOp lhs BinOpDivide rhs) -> do
        divide lhs rhs
        writeBack Asm.RAX name
    (Just name, BinOp lhs BinOpModulo rhs) -> do
        divide lhs rhs
        writeBack
         Asm.RDX name
    (Just name, BinOp lhs BinOpLess rhs) -> compare name lhs rhs Asm.FlagLess
    (Just name, BinOp lhs BinOpLessEqual rhs) -> compare name lhs rhs Asm.FlagLessEqual
    (Just name, BinOp lhs BinOpGreater rhs) -> compare name lhs rhs Asm.FlagGreater
    (Just name, BinOp lhs BinOpGreaterEqual rhs) -> compare name lhs rhs Asm.FlagGreaterEqual
    (Just name, BinOp lhs BinOpEqual rhs) -> compare name lhs rhs Asm.FlagEqual
    (Just name, BinOp lhs BinOpNotEqual rhs) -> compare name lhs rhs Asm.FlagNotEqual
    (Just name, BinOp lhs BinOpAnd rhs) -> simpleBinOp name lhs rhs Asm.And
    (Just name, BinOp lhs BinOpOr rhs) -> simpleBinOp name lhs rhs Asm.Or

    (Just name, UnOp UnOpNot arg) -> do
        reg <- lockInRegister name
        argOp <- getAsmOperand arg 
        emit $ Asm.Mov Asm.Mult8 argOp (Asm.OpRegister reg)
        emit $ Asm.Xor Asm.Mult8 (Asm.OpImmediate 1) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, UnOp UnOpNeg arg) -> do
        reg <- lockInRegister name
        argOp <- getAsmOperand arg 
        emit $ Asm.Mov Asm.Mult8 argOp (Asm.OpRegister reg)
        emit $ Asm.Neg Asm.Mult8 (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, GetAddr memory) -> do
        mem <- translateMemory memory
        reg <- lockInRegister name
        emit $ Asm.Lea Asm.Mult8 (Asm.OpMemory mem) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, Inc var size) -> do
        mem <- translateMemory var
        emit $ Asm.Inc (sizeToMult size) (Asm.OpMemory mem)

    (Just name, Dec var size) -> do
        mem <- translateMemory var
        emit $ Asm.Dec (sizeToMult size) (Asm.OpMemory mem)

    (Just name, IConst arg) -> do
        reg <- lockInRegister name
        op <- getAsmOperand arg
        emit $ Asm.Mov Asm.Mult8 op (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, _) -> fail "Invalid instruction"
  where
    simpleBinOp name lhs rhs f = do
        reg <- lockInRegister name

        lhsOp <- getAsmOperand lhs
        emit $ Asm.Mov Asm.Mult8 lhsOp (Asm.OpRegister reg)

        rhsOp <- getAsmOperand rhs
        emit $ f Asm.Mult8 rhsOp (Asm.OpRegister reg)

        writeBack reg name
        unlockRegisters

    divide lhs rhs = do
        lockInGivenRegister lhs Asm.RAX 
        emit $ Asm.Cltq
        lockRegister Asm.RDX

        rhsOp <- getAsmOperand rhs
        emit $ Asm.Idiv Asm.Mult8 rhsOp

        unlockRegisters

    compare name lhs rhs flag = do
        lhsOp <- getAsmOperand lhs
        rhsOp <- getAsmOperand rhs
        emit $ Asm.Cmp Asm.Mult8 rhsOp lhsOp
        unlockRegisters

        reg <- lockInRegister name
        emit $ Asm.Set flag (Asm.OpRegister reg)
        emit $ Asm.Movsbq (Asm.OpRegister reg) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

translateMemory :: MonadState EmitterState m => Memory -> m Asm.Memory
translateMemory (MemoryLocal _) = fail "No locals expected here"
translateMemory (MemoryArgument i)
    | Just reg <- regs ^? ix i = pure $ memoryOfRegister reg
    | otherwise = pure $ Asm.Memory Asm.RBP Nothing $ (2 + i - length regs) * 8
  where
    regs = [Asm.RDI, Asm.RSI, Asm.RDX, Asm.RCX, Asm.R8, Asm.R9]
translateMemory (MemoryOffset base _ Size0) = do
    baseReg <- lockInRegister base
    pure $ Asm.Memory baseReg Nothing 0
translateMemory (MemoryOffset base (OperandInt index) size) = do
    baseReg <- lockInRegister base
    pure $ Asm.Memory baseReg Nothing (index * sizeToInt size)
translateMemory (MemoryOffset base index size) = do
    baseReg <- lockInRegister base
    indexReg <- lockInRegister index
    pure $ Asm.Memory baseReg (Just (indexReg, mult)) 0
  where
    mult = case size of
        Size0 -> error "unreachable"
        Size8 -> Asm.Mult1
        Size32 -> Asm.Mult4
        Size64 -> Asm.Mult8
        SizePtr -> Asm.Mult8
translateMemory (MemoryGlobal ident) = pure . Asm.Global $ coerce ident
translateMemory MemoryUndef = fail "Undefined memory"

registerOrder :: [Asm.Register]
registerOrder =
    [ Asm.RDI, Asm.RSI, Asm.R8, Asm.R9, Asm.R10, Asm.R11, Asm.RAX
    , Asm.RCX, Asm.RDX, Asm.RBX, Asm.R12, Asm.R13, Asm.R14, Asm.R15
    ]

memoryOfRegister :: Asm.Register -> Asm.Memory
memoryOfRegister = (map Map.!)
  where
    map = Map.fromList $ [(reg, Asm.Memory Asm.RBP Nothing (-8 * i)) | (reg, i) <- zip registerOrder [2..]]

memoryOfSpill :: Int -> Asm.Memory
memoryOfSpill i = Asm.Memory Asm.RBP Nothing (-8 * (i + length registerOrder))

blockIdent :: Block -> Asm.Ident
blockIdent block = coerce . nameToIdent $ block ^. blockName

phiBlockIdent :: Block -> Block -> Asm.Ident
phiBlockIdent from to = coerce $ BS.concat
    [coerce  . nameToIdent $ from ^. name, "_phi_", coerce . nameToIdent $ to ^. name]

class GetAsmOperand a where
    getAsmOperand :: MonadState EmitterState m => a -> m Asm.Operand

instance GetAsmOperand Name where
    getAsmOperand name = use (esPreferredLocations . at name . singular _Just) >>= \case
        Asm.RSSpill i -> pure . Asm.OpMemory $ memoryOfSpill i
        Asm.RSRegister reg -> use (esRegisterState . at reg . singular _Just) >>= \case
            RSMem -> pure . Asm.OpMemory $ memoryOfRegister reg
            _ -> pure $ Asm.OpRegister reg

instance GetAsmOperand Int where
    getAsmOperand = pure . Asm.OpImmediate

instance GetAsmOperand Size where
    getAsmOperand = pure . Asm.OpImmediate . sizeToInt

instance GetAsmOperand Operand where
    getAsmOperand (OperandNamed name) = getAsmOperand name
    getAsmOperand (OperandSize size) = getAsmOperand size
    getAsmOperand (OperandInt i) = getAsmOperand i
    getAsmOperand OperandUndef = fail "undefined operand"

class LockInRegister a where
    lockInGivenRegister :: MonadState EmitterState m => a -> Asm.Register -> m ()

    lockInRegister :: MonadState EmitterState m => a -> m Asm.Register
    lockInRegister x = do
        reg <- evictLockRegister
        lockInGivenRegister x reg
        pure reg

instance LockInRegister Name where
    lockInGivenRegister name reg = use (esPreferredLocations . at name . singular _Just) >>= \case
        Asm.RSSpill i -> do
            emit $ Asm.Mov Asm.Mult8 (Asm.OpMemory $ memoryOfSpill i) (Asm.OpRegister reg)
        Asm.RSRegister reg' ->
            emit $ Asm.Mov Asm.Mult8 (Asm.OpMemory $ memoryOfRegister reg') (Asm.OpRegister reg)

    lockInRegister name = use (esPreferredLocations . at name . singular _Just) >>= \case
        Asm.RSSpill _ -> do
            reg <- evictLockRegister
            lockInGivenRegister name reg
            pure reg
        Asm.RSRegister reg -> use (esLockedRegisters . contains reg) >>= \case
            False -> do
                restoreRegister reg
                esLockedRegisters . contains reg .= True
                pure reg
            True -> do
                reg' <- evictLockRegister
                lockInGivenRegister name reg'
                pure reg'

instance LockInRegister Int where
    lockInGivenRegister i reg = emit $ Asm.Mov Asm.Mult8 (Asm.OpImmediate i) (Asm.OpRegister reg)

instance LockInRegister Operand where
    lockInRegister (OperandNamed name) = lockInRegister name
    lockInRegister (OperandSize size) = lockInRegister $ sizeToInt size
    lockInRegister (OperandInt i) = lockInRegister i
    lockInRegister OperandUndef = lockInRegister 0

    lockInGivenRegister (OperandNamed name) = lockInGivenRegister name
    lockInGivenRegister (OperandSize _) = lockInGivenRegister 64 -- TODO
    lockInGivenRegister (OperandInt i) = lockInGivenRegister i
    lockInGivenRegister OperandUndef = lockInGivenRegister 0

writeBack :: MonadState EmitterState m => Asm.Register -> Name -> m ()
writeBack reg name = use (esPreferredLocations . at name . singular _Just) >>= \case
    Asm.RSSpill i -> 
        emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfSpill i)
    Asm.RSRegister reg' -> unless (reg == reg') $
        emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfRegister reg')

saveRegister :: MonadState EmitterState m => Asm.Register -> m ()
saveRegister reg = use (esRegisterState . at reg . singular _Just) >>= \case
    RSRegMem -> pure ()
    RSMem -> pure ()
    RSReg -> do
        emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfRegister reg)
        esRegisterState . at reg ?= RSRegMem

restoreRegister :: MonadState EmitterState m => Asm.Register -> m ()
restoreRegister reg = use (esRegisterState . at reg . singular _Just) >>= \case
    RSRegMem -> pure ()
    RSReg -> pure ()
    RSMem -> do
        emit $ Asm.Mov Asm.Mult8 (Asm.OpMemory $ memoryOfRegister reg) (Asm.OpRegister reg)
        esRegisterState . at reg ?= RSRegMem

markRegisterDirty :: MonadState EmitterState m => Asm.Register -> m ()
markRegisterDirty reg = esRegisterState . at reg ?= RSReg

-- TODO
evictLockRegister :: MonadState EmitterState m => m Asm.Register
evictLockRegister = do
    locked <- use esLockedRegisters
    let reg = last [reg | reg <- registerOrder, not $ Set.member reg locked]
    lockRegister reg
    pure reg

lockRegister :: MonadState EmitterState m => Asm.Register -> m ()
lockRegister reg = do
    saveRegister reg
    esRegisterState . at reg ?= RSMem
    esLockedRegisters . contains reg .= True

sizeToInt :: Size -> Int
sizeToInt Size0 = 0
sizeToInt Size8 = 8
sizeToInt Size32 = 32
sizeToInt Size64 = 64
sizeToInt SizePtr = 64

sizeToMult :: Size -> Asm.Multiplier
sizeToMult Size0 = Asm.Mult1
sizeToMult Size8 = Asm.Mult1
sizeToMult Size32 = Asm.Mult4
sizeToMult Size64 = Asm.Mult8
sizeToMult SizePtr = Asm.Mult8

unlockRegisters :: MonadState EmitterState m => m ()
unlockRegisters = esLockedRegisters .= Set.empty
