{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.ShrinkEnds (opt) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = use meFunctions >>= mapM_ runFunction

runFunction :: MonadIO m => FunctionDescriptor -> m ()
runFunction func = liftIO $ do
    blocks <- reachableBlocks (func ^. funcEntryBlock)
    preds <- predecessors blocks
    traverse_ (go preds) $ reverse blocks
  where
    go preds block = readIORef (block ^. blockEnd) >>= \case
        BlockEndBranch succ | Just [_] <- Map.lookup succ preds -> merge block succ
        _ -> pure () 

    merge block succ = do
        succPhi <- readIORef $ succ ^. blockPhi
        when (Seq.null succPhi) $ do
            succBody <- readIORef $ succ ^. blockBody
            modifyIORef (block ^. blockBody) (Seq.>< succBody)
            readIORef (succ ^. blockEnd) >>= writeIORef (block ^. blockEnd)
