{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.Propagate (opt) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = use meFunctions >>= mapM_ runFunction

type Substitution = Map.Map Name Operand

runFunction :: MonadIO m => FunctionDescriptor -> m ()
runFunction func = do
    blocks <- reachableBlocks (func ^. funcEntryBlock)
    subst <- execStateT (mapM_ runBody blocks) Map.empty
    mapM_ (finishBlock subst) blocks

runBody :: (MonadIO m, MonadState Substitution m) => Block -> m ()
runBody block = liftIO (readIORef ioref) >>= foldlM runInstruction Seq.empty >>= liftIO . writeIORef ioref
  where
    ioref = block ^. blockBody

finishBlock :: MonadIO m => Substitution -> Block -> m ()
finishBlock subst block = liftIO $ do
    modifyIORef' (block ^. blockPhi) (applySubst subst)
    modifyIORef' (block ^. blockEnd) (applySubst subst)

runInstruction :: (MonadIO m, MonadState Substitution m) => Seq.Seq Instruction -> Instruction -> m (Seq.Seq Instruction)
runInstruction acc instr = do
    instr' <- gets $ flip applySubst instr
    case views instrPayload valueOf instr' of
        Nothing -> pure $ acc Seq.|> instr'
        Just op -> do
            forM_ (instr ^. instrResult) $ \name -> at name ?= op
            pure acc
  where
    valueOf (IConst op) = Just op
    valueOf (BinOp (Operand (OperandInt 0) _) BinOpPlus o) = Just o
    valueOf (BinOp o BinOpPlus (Operand (OperandInt 0) _)) = Just o
    valueOf (BinOp o BinOpMinus (Operand (OperandInt 0) _)) = Just o
    valueOf (BinOp (Operand (OperandInt 0) sz) BinOpTimes _) = Just $ Operand (OperandInt 0) sz
    valueOf (BinOp _ BinOpTimes (Operand (OperandInt 0) sz)) = Just $ Operand (OperandInt 0) sz
    valueOf (BinOp _ BinOpDivide (Operand (OperandInt 0) _)) = Just $ Operand OperandUndef SizePtr -- undefined behaviour
    valueOf (BinOp _ BinOpModulo (Operand (OperandInt 0) _)) = Just $ Operand OperandUndef SizePtr -- undefined behaviour
    valueOf (BinOp (Operand (OperandInt lhs) sz) op (Operand (OperandInt rhs) _)) = Just $ Operand (OperandInt value) sz
      where
        iverson True = 1
        iverson False = 0

        value = case op of
            BinOpPlus -> lhs + rhs
            BinOpMinus -> lhs - rhs
            BinOpTimes -> lhs * rhs
            BinOpDivide -> lhs `div` rhs
            BinOpModulo -> lhs `mod` rhs
            BinOpLess -> iverson $ lhs < rhs
            BinOpLessEqual -> iverson $ lhs <= rhs
            BinOpGreater -> iverson $ lhs > rhs
            BinOpGreaterEqual -> iverson $ lhs >= rhs
            BinOpEqual -> iverson $ lhs == rhs
            BinOpNotEqual -> iverson $ lhs /= rhs
            BinOpAnd -> iverson $ (lhs /= 0) && (rhs /= 0)
            BinOpOr -> iverson $ (lhs /= 0) || (rhs /= 0)
    valueOf (UnOp UnOpNeg (Operand (OperandInt i) sz)) = Just $ Operand (OperandInt $ -i) sz
    valueOf (UnOp UnOpNot (Operand (OperandInt i) sz)) = Just $ Operand (OperandInt $ 1 - i) sz
    valueOf _ = Nothing

applySubst :: HasOperands a => Substitution -> a -> a
applySubst subst = operands %~ mut
  where
    mut o@(Operand (OperandNamed name) _) = fromMaybe o $ Map.lookup name subst
    mut o = o
