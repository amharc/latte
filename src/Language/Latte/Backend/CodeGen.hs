{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-type-defaults #-}
module Language.Latte.Backend.CodeGen (emitState) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Coerce
import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Language.Latte.Backend.Asm as Asm
import Language.Latte.Middleend.Monad
import Language.Latte.Middleend.IR
import qualified Language.Latte.Middleend.DataflowAnalysisEngine as DAE
import qualified Language.Latte.Middleend.DeadCodeElimination as LV
import Language.Latte.Backend.RegAlloc

default (Int)

data EmitterState = EmitterState
    { _esInstructions :: Seq.Seq Asm.Instruction
    , _esRegisterState :: Map.Map Asm.Register RegisterState
    , _esPreferredLocations :: Map.Map Name Asm.RegisterOrSpill
    , _esNumLocations :: Int
    , _esSavedRegsStart :: Int
    , _esLockedRegisters :: Set.Set Asm.Register
    , _esLiveAfterPhi :: Map.Map Block (Set.Set Name)
    , _esLiveAfterBody :: Map.Map Block (Set.Set Name)
    , _esLiveAfterEnd :: Map.Map Block (Set.Set Name)
    , _esInFlags :: Maybe (Asm.Flag, Name)
    }

data RegisterState = RSReg | RSMem | RSRegMem | RSNone

makeLenses ''EmitterState
makePrisms ''RegisterState

emitState :: MonadIO m => MiddleEndState -> m (Seq.Seq Asm.Instruction)
emitState st = do
    funcs <- fold <$> itraverse emitFunction (st ^. meFunctions)
    strgs <- fold <$> itraverse emitString (st ^. meStrings)
    objs <- fold <$> itraverse emitObject (st ^. meObjects)
    pure $ funcs <> strgs <> objs

-- |Emits a function
--
-- Precondition: valid SSA, mem2reg applied
emitFunction :: MonadIO m => Ident -> FunctionDescriptor -> m (Seq.Seq Asm.Instruction)
emitFunction functionName desc = do
    blocks <- reachableBlocks entryBlock
    regAlloc <- allocateRegisters blocks
    liveAfterEnd <- DAE.runDAEBlocks blocks
    liveAfterPhi <- calcLiveAfterPhi liveAfterEnd
    liveAfterBody <- calcLiveAfterBody liveAfterEnd
    arguments <- argumentsOfEntryBlock entryBlock
    evalStateT (do
        emit $ Asm.Section ".text"
        emit $ Asm.Type (coerce functionName) "@function"
        emit . Asm.GlobalFunc $ coerce functionName
        emit . Asm.Label $ coerce functionName
        prologue
        forM_ blocks translateBlock
        use $ esInstructions
        )
        EmitterState
            { _esInstructions = Seq.empty
            , _esRegisterState = Map.fromList [(reg, RSReg) | reg <- registerOrder]
            , _esPreferredLocations = preprocessLocations <$> regAlloc
            , _esNumLocations = foldl max 0 regAlloc
            , _esSavedRegsStart = 16 * ((foldl max (length registerOrder) regAlloc + 1) `div` 2) + 8
            , _esLockedRegisters = Set.empty
            , _esLiveAfterPhi = liveAfterPhi
            , _esLiveAfterEnd = LV.getLiveVariables <$> liveAfterEnd
            , _esLiveAfterBody = liveAfterBody
            , _esInFlags = Nothing
            }
  where
    preprocessLocations i
        | Just reg <- registerOrder ^? ix i = Asm.RSRegister reg
        | otherwise = Asm.RSSpill (i - length registerOrder)

    entryBlock = desc ^. funcEntryBlock

    calcLiveAfterPhi live = liftIO . iforM live $ \block liveAfter -> do
        end <- readIORef $ block ^. blockEnd
        body <- readIORef $ block ^. blockBody

        pure . LV.getLiveVariables $ foldr (flip DAE.stepInstruction) (DAE.stepEnd liveAfter end) body

    calcLiveAfterBody live = liftIO . iforM live $ \block liveAfter -> do
        end <- readIORef $ block ^. blockEnd

        pure . LV.getLiveVariables $ DAE.stepEnd liveAfter end


emitString :: MonadIO m => Ident -> BS.ByteString -> m (Seq.Seq Asm.Instruction)
emitString ident str = pure
    [ Asm.Section ".rodata"
    , Asm.Align 8
    , Asm.Type (coerce ident) "@object"
    , Asm.Label (coerce ident)
    , Asm.String str
    ]

emitObject :: MonadIO m => Ident -> Object -> m (Seq.Seq Asm.Instruction)
emitObject ident obj = pure $ start Seq.>< end
  where
    start = 
        [ Asm.Section ".rodata"
        , Asm.Align 8
        , Asm.Type (coerce ident) "@object"
        , Asm.Label (coerce ident)
        ]
    end = flip foldMap (obj ^. objectFields) $ \field ->
        case field ^. objectFieldData of
            ObjectFieldInt i ->
                [Asm.QuadInt i]
            ObjectFieldNull ->
                [Asm.QuadInt 0]
            ObjectFieldRef ref ->
                [Asm.Quad (coerce ref)]

argumentsOfEntryBlock :: MonadIO m => Block -> m Int
argumentsOfEntryBlock entryBlock = do
    body <- liftIO . readIORef $ entryBlock ^. blockBody
    pure . fromMaybe 0 $ maximumOf (traverse . instrPayload . _ILoad . loadFrom . _MemoryArgument) body

emit :: MonadState EmitterState m => Asm.Instruction -> m ()
emit instr = esInstructions %= flip snoc instr

translateBlock :: (MonadIO m, MonadState EmitterState m) => Block -> m ()
translateBlock block = do
    emit . Asm.Label $ blockIdent block
    emit $ Asm.AnnotateLiveStop

    esRegisterState . traverse .= RSNone
    esInFlags .= Nothing
    use (esLiveAfterPhi . at block . singular _Just) >>= mapM_ prepareVar

    body <- liftIO . readIORef $ block ^. blockBody
    forM_ body translateInstr

    liftIO (readIORef $ block ^. blockEnd) >>= \case
        BlockEndNone -> pure ()
        BlockEndReturn arg -> do
            use (esLiveAfterBody . at block . singular _Just) >>= annotateLive
            safeLockInGivenRegister arg Asm.RAX
            epilogue
        BlockEndReturnVoid -> do
            use (esLiveAfterBody . at block . singular _Just) >>= annotateLive
            epilogue
        BlockEndBranch target -> do
            regs <- use esRegisterState

            use (esLiveAfterBody . at block . singular _Just) >>= annotateLive
            emit $ Asm.Jump (phiBlockIdent block target)
            linkTo target

            esRegisterState .= regs
        BlockEndBranchCond cond ifTrue ifFalse -> do
            use esInFlags >>= \case
                Just (flag, name) | OperandNamed name == cond ^. operandPayload -> do
                    use (esLiveAfterEnd . at block . singular _Just) >>= annotateLive
                    emit $ Asm.JumpCond flag (phiBlockIdent block ifTrue)
                _ -> do
                    use (esLiveAfterBody . at block . singular _Just) >>= annotateLive
                    op <- getAsmOperand cond
                    emit $ Asm.Test Asm.Mult1 (Asm.OpImmediate 1) op
                    emit $ Asm.JumpCond Asm.FlagNotEqual (phiBlockIdent block ifTrue)

            emit $ Asm.Jump (phiBlockIdent block ifFalse)

            regs <- use esRegisterState

            linkTo ifFalse
            esRegisterState .= regs

            linkTo ifTrue
            esRegisterState .= regs

    unlockRegisters
  where
    linkTo target = do
        emit . Asm.Label $ phiBlockIdent block target
        phis <- liftIO . readIORef $ target ^. blockPhi
        forM_ phis $ \phi ->
            forM_ (phi ^. phiBranches) $ \branch ->        
                when (branch ^. phiFrom == block) $ do
                    reg <- lockInRegister $ phi ^. name
                    op <- getAsmOperand (branch ^. phiValue)
                    emit $ Asm.Mov Asm.Mult8 op (Asm.OpRegister reg)
                    writeBack reg (phi ^. name)
                    unlockRegisters

        use (esLiveAfterPhi . at target . singular _Just) >>= mapM_ restoreVar
        emit . Asm.Jump $ blockIdent target

    restoreVar var = use (esPreferredLocations . at var . singular _Just) >>= \case
        Asm.RSRegister reg -> restoreRegister reg
        _ -> pure ()

    prepareVar var = use (esPreferredLocations . at var . singular _Just) >>= \case
        Asm.RSRegister reg -> esRegisterState . at reg ?= RSReg
        _ -> pure ()

    calcLiveRegs = flip foldlM Set.empty $ \acc var -> use (esPreferredLocations . at var . singular _Just) >>= \case
        Asm.RSRegister reg -> pure $ Set.insert reg acc
        _ -> pure acc

    annotateLive = calcLiveRegs >=> emit . Asm.AnnotateLiveStart

prologue :: MonadState EmitterState m => m ()
prologue = do
    emit $ Asm.Push Asm.Mult8 (Asm.OpRegister Asm.RBP)
    emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister Asm.RSP) (Asm.OpRegister Asm.RBP)
    srs <- use esSavedRegsStart
    emit $ Asm.Sub Asm.Mult8 (Asm.OpImmediate srs) (Asm.OpRegister Asm.RSP)
    forM_ savedRegs $ emit . Asm.Push Asm.Mult8 . Asm.OpRegister

epilogue :: MonadState EmitterState m => m ()
epilogue = do
    forM_ (reverse savedRegs) $ emit . Asm.Pop Asm.Mult8 . Asm.OpRegister
    emit Asm.Leave
    emit Asm.Ret

savedRegs :: [Asm.Register]
savedRegs = [Asm.RDI, Asm.RSI, Asm.RDX, Asm.RCX, Asm.R8, Asm.R9, Asm.RBX, Asm.R12, Asm.R13, Asm.R14, Asm.R15]

translateInstr :: MonadState EmitterState m => Instruction -> m ()
translateInstr instr = case (instr ^. instrResult, instr ^. instrPayload) of
    (Nothing, Call dest args) -> call dest args
    (Nothing, IIntristic _) -> fail "Ignoring the result of an intristic, probably a bug"
    (Nothing, Store to size (Operand (OperandInt i) _)) -> do
        mem <- translateMemory to
        emit $ Asm.Mov (sizeToMult size) (Asm.OpImmediate i) (Asm.OpMemory mem)
    (Nothing, Store to size (Operand (OperandSize sz) _)) -> do
        mem <- translateMemory to
        emit $ Asm.Mov (sizeToMult size) (Asm.OpImmediate $ sizeToInt sz) (Asm.OpMemory mem)
    (_, Store to size value) -> do
        reg <- lockInRegister value
        mem <- translateMemory to
        emit $ Asm.Mov (sizeToMult size) (Asm.OpRegister reg) (Asm.OpMemory mem)
        unlockRegisters

    (Just name, Call dest args) -> do
        call dest args
        writeBack Asm.RAX name

    (Just name, IIntristic (IntristicAlloc length ty)) -> do
        -- TODO types
        call (MemoryGlobal "alloc") [length]
        writeBack Asm.RAX name

    (Just name, IIntristic (IntristicClone prototype fields)) -> do
        call (MemoryGlobal "clone_object") [prototype, Operand (OperandInt $ 8 * fields) Size32]
        writeBack Asm.RAX name

    (Just name, IIntristic (IntristicConcat lhs rhs)) -> do
        call (MemoryGlobal "concat") [lhs, rhs]
        writeBack Asm.RAX name

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
        writeBack Asm.RDX name
    (Just name, BinOp lhs BinOpLess rhs) -> compare name lhs rhs Asm.FlagLess
    (Just name, BinOp lhs BinOpLessEqual rhs) -> compare name lhs rhs Asm.FlagLessEqual
    (Just name, BinOp lhs BinOpGreater rhs) -> compare name lhs rhs Asm.FlagGreater
    (Just name, BinOp lhs BinOpGreaterEqual rhs) -> compare name lhs rhs Asm.FlagGreaterEqual
    (Just name, BinOp lhs BinOpEqual rhs) -> compare name lhs rhs Asm.FlagEqual
    (Just name, BinOp lhs BinOpNotEqual rhs) -> compare name lhs rhs Asm.FlagNotEqual
    (Just name, BinOp lhs BinOpAnd rhs) -> simpleBinOp name lhs rhs Asm.And
    (Just name, BinOp lhs BinOpOr rhs) -> simpleBinOp name lhs rhs Asm.Or
    (Just name, BinOp lhs BinOpShiftLeft rhs) -> simpleBinOp name lhs rhs Asm.Sal
    (Just name, BinOp lhs BinOpShiftRight rhs) -> simpleBinOp name lhs rhs Asm.Sar

    (Just name, UnOp UnOpNot arg) -> do
        reg <- lockInRegister name
        argOp <- getAsmOperand arg 
        emit $ Asm.Mov (sizeToMult $ arg ^. operandSize) argOp (Asm.OpRegister reg)
        emit $ Asm.Xor (sizeToMult $ arg ^. operandSize) (Asm.OpImmediate 1) (Asm.OpRegister reg)
        esInFlags .= Nothing
        writeBack reg name
        unlockRegisters

    (Just name, UnOp UnOpNeg arg) -> do
        reg <- lockInRegister name
        argOp <- getAsmOperand arg 
        emit $ Asm.Mov (sizeToMult $ arg ^. operandSize) argOp (Asm.OpRegister reg)
        emit $ Asm.Neg (sizeToMult $ arg ^. operandSize) (Asm.OpRegister reg)
        esInFlags .= Nothing
        writeBack reg name
        unlockRegisters

    (Just name, GetAddr memory) -> do
        mem <- translateMemory memory
        reg <- lockInRegister name
        emit $ Asm.Lea Asm.Mult8 (Asm.OpMemory mem) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    (_, Inc var size) -> do
        mem <- translateMemory var
        emit $ Asm.Inc (sizeToMult size) (Asm.OpMemory mem)
        esInFlags .= Nothing

    (_, Dec var size) -> do
        mem <- translateMemory var
        emit $ Asm.Dec (sizeToMult size) (Asm.OpMemory mem)
        esInFlags .= Nothing

    (Just name, IConst arg) -> do
        reg <- lockInRegister name
        op <- getAsmOperand arg
        emit $ Asm.Mov Asm.Mult8 op (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    _ -> error "unreachable"
  where
    simpleBinOp name lhs rhs f = do
        reg <- lockInRegister name

        lhsOp <- getAsmOperand lhs
        emit $ Asm.Mov (sizeToMult $ lhs ^. operandSize) lhsOp (Asm.OpRegister reg)

        rhsOp <- getAsmOperand rhs
        emit $ f (sizeToMult $ rhs ^. operandSize) rhsOp (Asm.OpRegister reg)
        esInFlags .= Nothing

        writeBack reg name
        unlockRegisters

    divide lhs rhs = do
        lockRegister Asm.RDX
        safeLockInGivenRegister lhs Asm.RAX 
        emit $ Asm.Cqto

        rhsReg <- lockInRegister rhs
        emit $ Asm.Idiv (sizeToMult $ rhs ^. operandSize) (Asm.OpRegister rhsReg)

        unlockRegisters

    compare name lhs rhs flag = do
        lhsReg <- lockInRegister lhs
        rhsReg <- lockInRegister rhs
        emit $ Asm.Cmp (sizeToMult $ lhs ^. operandSize) (Asm.OpRegister rhsReg) (Asm.OpRegister lhsReg)
        esInFlags ?= (flag, name)
        unlockRegisters

        reg <- lockInRegister name
        emit $ Asm.Set flag (Asm.OpRegister reg)
        --emit $ Asm.Movsbq (Asm.OpRegister reg) (Asm.OpRegister reg)
        writeBack reg name
        unlockRegisters

    call target args = do
        mapM_ clobberRegister ([Asm.RAX, Asm.RCX, Asm.RDX, Asm.RSI, Asm.RDI, Asm.R8, Asm.R9, Asm.R10, Asm.R11] :: [Asm.Register])
        zipWithM_ safeLockInGivenRegister args [Asm.RDI, Asm.RSI, Asm.RDX, Asm.RCX, Asm.R8, Asm.R9]
        op <- translateMemory target
        lockRegister Asm.RAX
        emit $ Asm.Call (Asm.OpMemory op)
        esInFlags .= Nothing
        unlockRegisters

translateMemory :: MonadState EmitterState m => Memory -> m Asm.Memory
translateMemory (MemoryLocal _) = fail "No locals expected here"
translateMemory (MemoryArgument i)
    | i < 6 = do
        srs <- use esSavedRegsStart
        pure $ Asm.Memory Asm.RBP Nothing (- srs - i * 8 - 8)
    | otherwise = pure $ Asm.Memory Asm.RBP Nothing $ (2 + i - length regs) * 8
  where
    regs :: [Asm.Register]
    regs = [Asm.RDI, Asm.RSI, Asm.RDX, Asm.RCX, Asm.R8, Asm.R9]
translateMemory (MemoryOffset base _ Size0) = do
    baseReg <- lockInRegister base
    pure $ Asm.Memory baseReg Nothing 0
translateMemory (MemoryOffset base (Operand (OperandInt index) _) size) = do
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
    [ Asm.R12, Asm.R13, Asm.R14, Asm.R15, Asm.RBX, Asm.RAX, Asm.RDX
    , Asm.RCX, Asm.RDI, Asm.RSI, Asm.R8, Asm.R9, Asm.R10, Asm.R11
    ]

memoryOfRegister :: Asm.Register -> Asm.Memory
memoryOfRegister = (map Map.!)
  where
    map = Map.fromList $ [(reg, Asm.Memory Asm.RBP Nothing (-8 * i - 8)) | (reg, i) <- zip registerOrder [2..]]

memoryOfSpill :: Int -> Asm.Memory
memoryOfSpill i = Asm.Memory Asm.RBP Nothing (-8 * (i + length registerOrder) - 8)

blockIdent :: Block -> Asm.Ident
blockIdent block = coerce . nameToIdent $ block ^. blockName

phiBlockIdent :: Block -> Block -> Asm.Ident
phiBlockIdent from to = coerce $ BS.concat
    [coerce  . nameToIdent $ from ^. name, "_phi_", coerce . nameToIdent $ to ^. name]

class GetAsmOperand a where
    getAsmOperand :: MonadState EmitterState m => a -> m Asm.Operand

instance GetAsmOperand Name where
    getAsmOperand name = use (esPreferredLocations . at name) >>= \case
        Just (Asm.RSSpill i) -> pure . Asm.OpMemory $ memoryOfSpill i
        Just (Asm.RSRegister reg) -> use (esRegisterState . at reg) >>= \case
            Just RSMem -> pure . Asm.OpMemory $ memoryOfRegister reg
            Just _ -> pure $ Asm.OpRegister reg
            Nothing -> fail "No register state"
        Nothing -> fail "No preferred location for name"

instance GetAsmOperand Int where
    getAsmOperand = pure . Asm.OpImmediate

instance GetAsmOperand Size where
    getAsmOperand = pure . Asm.OpImmediate . sizeToInt

instance GetAsmOperand Operand where
    getAsmOperand operand = getAsmOperand $ operand ^. operandPayload

instance GetAsmOperand OperandPayload where
    getAsmOperand (OperandNamed name) = getAsmOperand name
    getAsmOperand (OperandSize size) = getAsmOperand size
    getAsmOperand (OperandInt i) = getAsmOperand i
    getAsmOperand OperandUndef = fail "undefined operand"

class LockInRegister a where
    lockInGivenRegister :: MonadState EmitterState m => a -> Asm.Register -> m ()

    safeLockInGivenRegister :: MonadState EmitterState m => a -> Asm.Register -> m ()
    safeLockInGivenRegister x reg = do
        saveRegister reg
        lockInGivenRegister x reg

    lockInRegister :: MonadState EmitterState m => a -> m Asm.Register
    lockInRegister x = do
        reg <- evictLockRegister
        lockInGivenRegister x reg
        pure reg

instance LockInRegister Name where
    lockInGivenRegister name reg = use (esPreferredLocations . at name . singular _Just) >>= \case
        Asm.RSSpill i -> do
            emit $ Asm.Mov Asm.Mult8 (Asm.OpMemory $ memoryOfSpill i) (Asm.OpRegister reg)
        Asm.RSRegister reg' -> use (esRegisterState . at reg') >>= \case
            Just RSReg -> emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg') (Asm.OpRegister reg)
            Just _ -> emit $ Asm.Mov Asm.Mult8 (Asm.OpMemory $ memoryOfRegister reg') (Asm.OpRegister reg)
            Nothing -> fail "No register state for register"

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
    lockInRegister operand = lockInRegister $ operand ^. operandPayload
    lockInGivenRegister operand = lockInGivenRegister (operand ^. operandPayload)

instance LockInRegister OperandPayload where
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
    Asm.RSRegister reg'
        | reg == reg' -> markRegisterDirty reg
        | otherwise -> use (esRegisterState . at reg' . singular _Just) >>= \case
            RSMem -> emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfRegister reg')
            _ -> do
                markRegisterDirty reg'
                emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpRegister reg')

saveRegister :: MonadState EmitterState m => Asm.Register -> m ()
saveRegister reg = use (esRegisterState . at reg . singular _Just) >>= \case
    RSRegMem -> pure ()
    RSMem -> pure ()
    RSNone -> pure ()
    RSReg -> do
        emit $ Asm.Mov Asm.Mult8 (Asm.OpRegister reg) (Asm.OpMemory $ memoryOfRegister reg)
        esRegisterState . at reg ?= RSRegMem

clobberRegister :: MonadState EmitterState m => Asm.Register -> m ()
clobberRegister reg = do
    saveRegister reg
    esRegisterState . at reg ?= RSMem

restoreRegister :: MonadState EmitterState m => Asm.Register -> m ()
restoreRegister reg = use (esRegisterState . at reg . singular _Just) >>= \case
    RSRegMem -> pure ()
    RSReg -> pure ()
    RSNone -> pure ()
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
sizeToInt Size8 = 1
sizeToInt Size32 = 4
sizeToInt Size64 = 8
sizeToInt SizePtr = 8

sizeToMult :: Size -> Asm.Multiplier
sizeToMult Size0 = Asm.Mult1
sizeToMult Size8 = Asm.Mult1
sizeToMult Size32 = Asm.Mult4
sizeToMult Size64 = Asm.Mult8
sizeToMult SizePtr = Asm.Mult8

unlockRegisters :: MonadState EmitterState m => m ()
unlockRegisters = esLockedRegisters .= Set.empty
