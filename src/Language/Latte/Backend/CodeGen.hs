{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Latte.Backend.CodeGen where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Coerce
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Langauge.Latte.Backend.Asm as Asm
import qualified Langauge.Latte.Middleend.IR
import Language.Latte.Backend.RegAlloc

data EmitterState = EmitterState
    { _esInstructions :: Seq.Seq Asm.Instruction
    , _esNameSizes :: Map.Map Name Size
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
emitFunction :: MonadIO m => Ident -> Block -> m (Seq.Seq Asm)
emitFunction functionName entryBlock = do
    blocks <- reachableBlocks entryBlock
    preferredLocations <- allocateRegisters blocks
    arguments <- argumentsOfEntryBlock entryBlock

argumentsOfEntryBlock :: MonadIO m => Block -> m Int
argumentsOfEntryBlock entryBlock = do
    body <- liftIO . readIORef $ entryBlock ^. blockBody
    pure $ body ^. maximumOf (_ILoad . loadFrom . _MemoryArgument)

emit :: MonadState EmitterState m => Asm.Instruction -> m ()
emit instr = esInstructions %= flip snoc instr

translateInstr :: MonadState EmitterState m => Instruction -> m ()
translateInstr instr = case (instr ^. instrResult, instr ^. instrPayload) of
    (Nothing, ICall call) -> undefined
    (Nothing, IInstristic intristic) -> undefined
    (Nothing, SStore to size value) -> do
        reg <- lockInRegister value
        mem <- translateMemory from 
        emit $ Asm.Mov
    (Nothing, _) -> pure ()
    (Just name, SLoad from size) -> do
        reg <- lockInRegister name
        mem <- translateMemory from 
        emit $ Asm.Mov (sizeToMult size) (Asm.OpMemory mem) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, SBinOp lhs BinOpPlus rhs) -> simpleBinOp name lhs rhs Asm.Add
    (Just name, SBinOp lhs BinOpSub rhs) -> simpleBinOp name lhs rhs Asm.Sub
    (Just name, SBinOp lhs BinOpTimes rhs) -> simpleBinOp name lhs rhs Asm.Imul
    (Just name, SBinOp lhs BinOpDivide rhs) -> do
        divide lhs rhs
        writeBack Asm.RAX name
    (Just name, SBinOp lhs BinOpModulo rhs) -> do
        divide lhs rhs
        writeBack Asm.RDX name
    (Just name, SBinOp lhs BinOpLess rhs) -> compare name lhs rhs Asm.FlagLess
    (Just name, SBinOp lhs BinOpLessEqual rhs) -> compare name lhs rhs Asm.FlagLessEqual
    (Just name, SBinOp lhs BinOpGreater rhs) -> compare name lhs rhs Asm.FlagGreater
    (Just name, SBinOp lhs BinOpGreaterEqual rhs) -> compare name lhs rhs Asm.FlagGreaterEqual
    (Just name, SBinOp lhs BinOpEqual rhs) -> compare name lhs rhs Asm.FlagEqual
    (Just name, SBinOp lhs BinOpNotEqual rhs) -> compare name lhs rhs Asm.FlagNotEqual
    (Just name, SBinOp lhs BinOpAnd rhs) -> simpleBinOp name lhs rhs Asm.And
    (Just name, SBinOp lhs BinOpOr rhs) -> simpleBinOp name lhs rhs Asm.Or

    (Just name, SUnOp UnOpNot arg) -> do
        reg <- lockInRegister name
        argOp <- getOperand arg 
        emit $ Asm.Mov Asm.Mult8 argOp (Asm.OpRegister reg)
        emit $ Asm.Xor Asm.Mult8 (Asm.OpImmediate 1) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, SUnOp UnOpNeg arg) -> do
        reg <- lockInRegister name
        argOp <- getOperand arg 
        emit $ Asm.Mov Asm.Mult8 argOp (Asm.OpRegister reg)
        emit $ Asm.Neg Asm.Mult8 (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, SGetAddr memory) -> do
        mem <- translateMemory memory
        reg <- lockInRegister name
        emit $ Asm.Lea Asm.Mult8 (Asm.OpMemory mem) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (Just name, SInc mem size) -> do
        op <- getOperand mem
        emit $ Asm.Inc (sizeToMult size) op

    (Just name, SDec mem size) -> do
        op <- getOperand mem
        emit $ Asm.Dec (sizeToMult size) op

    (Just name, SConst arg) -> do
        reg <- lockInRegister name
        op <- getOperand arg
        emit $ Asm.Mov Asm.Mult8 op (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters
  where
    simpleBinOp name lhs rhs f = do
        reg <- lockInRegister name

        lhsOp <- getOperand lhs
        emit $ Asm.Mov Asm.Mult8 lhsOp (Asm.OpRegister reg)

        rhsOp <- getOperand rhs
        emit $ f Asm.Mult8 rhsOp (Asm.OpRegister reg)

        writeBack reg name
        unlockRegisters

    divide lhs rhs = do
        lockInGivenRegister lhs Asm.RAX 
        emit $ Asm.Cltq
        lockRegister Asm.RDX

        rhsOp <- getOperand rhs
        emit $ Asm.Idiv rhsOpF

        unlockRegisters

    compare name lhs rhs flag = do
        lhsOp <- getOperand lhs
        rhsOp <- getOperand rhs
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
    | Just reg <- regs ^. at i = memoryOfRegister reg
    | otherwise = pure $ Asm.Memory Asm.RBP Nothing $ (2 + i - length regs) * 8
  where
    regs = [RDI, RSI, RDX, RCX, R8, R9]
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
translateMemory (MemoryGlobal ident) = pure $ Asm.OpGlobal Ident
translateMemory MemoryUndef = fail "Undefined memory"

registerOrder :: [Asm.Register]
registerOrder =
    [ Asm.RDI, Asm.RSI, Asm.R8, Asm.R9, Asm.R10, Asm.R11, Asm.RAX
    , Asm.RCX, Asm.RDX, Asm.RBX, Asm.R12, Asm.R13, Asm.R14, Asm.R15
    ]

memoryOfRegister :: Asm.Register -> Asm.Memory
memoryOfRegister = (map Map.!)
  where
    map = Map.fromList $ [(reg, Memory RBP Nothing (-8 * i)) | (reg, i) <- zip registerOrder [2..]]

memoryOfSpill :: Int -> Asm.Memory
memoryOfSpill i = Asm.Memory Asm.RBP Nothing (-8 * (i + length registerOrder))

blockIdent :: Block -> Ident
blockIdent block = nameToIdent $ block ^. blockName

phiBlockIdent :: Block -> Block -> Ident
phiBlockIdent from to = coerce . BS.concat [nameToIdent from, "_phi_", nameToIdent to]

class GetAsmOperand a where
    getAsmOperand :: MonadState EmitterState m => a -> m Asm.Operand

instance GetAsmOperand Name where
    getAsmOperand name = use (esPreferredLocations . at name . singular _Just) >>= \case
        Asm.RSSpill i -> pure . Asm.OpMemory $ memoryOfSpill i
        Asm.RSRegister reg -> use (esRegisterState . at reg . singular _Just) >>= \case
            RsMem -> pure . Asm.OpMemory $ memoryOfRegister reg
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
    lockInGivenRegister :: MonadState EmitterState m => a -> Register -> m ()

    lockInRegister :: MonadState EmitterState m => a -> m Register
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

writeBack :: MonadState EmitterState m => Register -> Name -> m ()
writeBack reg name = use (esPreferredLocations . at name . singular _Just) >>= \case
    Asm.RSSpill i -> 
        emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfSpill i)
    Asm.RSRegister reg' -> unless (reg == reg') $
        emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfRegister reg')

saveRegister :: MonadState EmitterState m => Register -> m ()
saveRegister reg = use (esRegisterState . at reg . singular _Just) >>= \case
    RSRegMem -> pure ()
    RSMem -> pure ()
    RSReg -> do
        emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfRegister reg)
        esRegisterState ^. at reg ?= RSRegMem

restoreRegister :: MonadState EmitterState m => Register -> m ()
restoreRegister reg = use (esRegisterState . at reg . singular _Just) >>= \case
    RSRegMem -> pure ()
    RSReg -> pure ()
    RSMem -> do
        emit $ Asm.Mov Asm.Mult8 (Asm.OpMemory $ memoryOfRegister reg) (Asm.OpRegister reg)
        esRegisterState ^. at reg ?= RSRegMem

markRegisterDirty :: MonadState EmitterState m => Register -> m ()
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
    esRegisterState . at reg ?= RsMem
    esLockedRegisters . contains reg .= True

sizeToInt :: Size -> Int
sizeToInt Size0 = 0
sizeToInt Size8 = 8
sizeToInt Size32 = 32
sizeToInt Size64 = 64
sizeToInt SizePtr = 64

sizeToMult :: Size -> Asm.Mult
sizeToMult Size0 = Mult1
sizeToMult Size8 = Mult1
sizeToMult Size32 = Mult4
sizeToMult Size64 = Mult8
sizeToMult SizePtr = Mult8

unlockRegisters :: MonadState EmitterState m => m ()
unlockRegisters = esLockedRegisters .= Set.empty
