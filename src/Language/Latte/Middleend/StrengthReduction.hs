{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.StrengthReduction (opt) where

import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.IORef
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = use meFunctions >>= mapM_ runFunction

runFunction :: MonadIO m => FunctionDescriptor -> m ()
runFunction func = reachableBlocks (func ^. funcEntryBlock) >>= mapM_ runBlock

runBlock :: MonadIO m => Block -> m ()
runBlock block = liftIO $ modifyIORef' (block ^. blockBody) (fmap $ instrPayload %~ runInstruction)

runInstruction :: InstrPayload -> InstrPayload
runInstruction (BinOp lhs BinOpTimes (OpPow2 i)) = BinOp lhs BinOpShiftLeft (Operand (OperandInt i) Size64)
runInstruction (BinOp (OpPow2 i) BinOpTimes rhs) = BinOp rhs BinOpShiftLeft (Operand (OperandInt i) Size64)
runInstruction (BinOp lhs BinOpDivide (OpPow2 i)) = BinOp lhs BinOpShiftRight (Operand (OperandInt i) Size64)
runInstruction (BinOp lhs BinOpDivide (OpPow2 i)) = BinOp lhs BinOpShiftRight (Operand (OperandInt i) Size64)
runInstruction (BinOp lhs BinOpDivide MinusOne) = UnOp UnOpNeg lhs
runInstruction (BinOp lhs BinOpTimes MinusOne) = UnOp UnOpNeg lhs
runInstruction (BinOp MinusOne BinOpTimes rhs) = UnOp UnOpNeg rhs
runInstruction payload = payload

pattern OpPow2 :: Int -> Operand
pattern OpPow2 i <- Operand (OperandInt (ilog2 -> Just i)) _

pattern MinusOne :: Operand
pattern MinusOne <- Operand (OperandInt (-1)) _

ilog2 :: Int -> Maybe Int
ilog2 x | popCount x == 1 = Just $ countTrailingZeros x
ilog2 _ = Nothing
