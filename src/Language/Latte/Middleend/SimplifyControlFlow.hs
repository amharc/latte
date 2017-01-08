{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.SimplifyControlFlow (opt) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IORef
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = use meFunctions >>= mapM_ runFunction

runFunction :: MonadIO m => FunctionDescriptor -> m ()
runFunction func = reachableBlocks (func ^. funcEntryBlock) >>= mapM_ runBlock

runBlock :: MonadIO m => Block -> m ()
runBlock block = liftIO . modifyIORef (block ^. blockEnd) $ \case
    BlockEndBranchCond (Operand (OperandInt 0) _) _ ifFalse -> BlockEndBranch ifFalse
    BlockEndBranchCond (Operand (OperandInt _) _) ifTrue _ -> BlockEndBranch ifTrue
    BlockEndBranchCond _ ifTrue ifFalse | ifTrue == ifFalse -> BlockEndBranch ifTrue
    end -> end
