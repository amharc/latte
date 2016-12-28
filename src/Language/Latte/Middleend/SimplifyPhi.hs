{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.SimplifyPhi (opt) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = do
    funcs <- use meFunctions
    forM_ funcs runFunction

runFunction :: (MonadIO m) => FunctionDescriptor -> m ()
runFunction desc = reachableBlocks (desc ^. funcEntryBlock) >>= mapM_ runBlock

runBlock :: (MonadIO m) => Block -> m ()
runBlock block = liftIO $ readIORef ioref >>= updatePhis block >>= writeIORef ioref
  where
    ioref = block ^. blockPhi

updatePhis :: (MonadIO m) => Block -> Seq.Seq PhiNode -> m (Seq.Seq PhiNode)
updatePhis block = foldrM go Seq.empty
  where
    go node acc = (filterM incoming $ node ^. phiBranches) >>= \case
        [] -> push (node ^. name) OperandUndef >> pure acc
        [branch] -> push (node ^. name) (branch ^. phiValue) >> pure acc
        branches -> case Set.toList $ Set.fromList [branch ^. phiValue | branch <- branches] of
            [target] -> push (node ^. name) target >> pure acc
            _ -> pure ((node & phiBranches .~ branches) Seq.<| acc)

    incoming branch 
        | OperandUndef <- branch ^. phiValue = pure False
        | otherwise = do
            succs <- successors $ branch ^. phiFrom
            pure $ block `elem` succs

    push name value = liftIO $ modifyIORef (block ^. blockBody) (Instruction name (IConst value) [] Seq.<|)
        
