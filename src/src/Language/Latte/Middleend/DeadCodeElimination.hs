{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.DeadCodeElimination (opt, LiveVariables(..)) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad
import qualified Language.Latte.Middleend.DataflowAnalysisEngine as DAE
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

newtype LiveVariables = LiveVariables { getLiveVariables :: Set.Set Name }
    deriving (Eq, Show)

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = do
    liveMap <- DAE.runDAE
    use meFunctions >>= mapM_ (runFunction liveMap)

runFunction :: MonadIO m => Map.Map Block LiveVariables -> FunctionDescriptor -> m ()
runFunction live func = reachableBlocks (func ^. funcEntryBlock) >>= traverse_ (runBlock live)

runBlock :: MonadIO m => Map.Map Block LiveVariables -> Block -> m ()
runBlock liveMap block = liftIO $ do
    phi <- readIORef $ block ^. blockPhi
    body <- readIORef $ block ^. blockBody
    end <- readIORef $ block ^. blockEnd

    liveBeforePhiTargets <- DAE.stepPhiSource liveAfter block

    let (body', liveBeforeBody) = foldr goInstr (Seq.empty, step liveBeforePhiTargets end) body
        phi' = Seq.filter (not . flip isRemovable liveBeforeBody) phi

    writeIORef (block ^. blockPhi) phi'
    writeIORef (block ^. blockBody) body'
  where
    liveAfter = liveMap Map.! block

    goInstr :: Instruction -> (Seq.Seq Instruction, LiveVariables) -> (Seq.Seq Instruction, LiveVariables)
    goInstr item (acc, live)
        | isRemovable item live = (acc, live)
        | Just name <- item ^. instrResult
        , isLive name live = (item Seq.<| acc, step live item)
        | otherwise = ((item & instrResult .~ Nothing) Seq.<| acc, step live item)

instance DAE.DAE LiveVariables where
#if __GLASGOW_HASKELL__ >= 801
    direction = DAE.DAEBackward
    postProcess = DAE.DAEMerge
#else
    direction _ = DAE.DAEBackward
    postProcess _ = DAE.DAEMerge
#endif
    stepInstruction = step
    stepPhiBranch = stepSimple
    stepPhiTarget (LiveVariables live) phis = LiveVariables $ foldr Set.delete live (phis ^.. folded . names)
    stepEnd = step

instance Monoid LiveVariables where
    mempty = LiveVariables Set.empty
    LiveVariables lhs `mappend` LiveVariables rhs = LiveVariables $ Set.union lhs rhs

step :: (HasOperands a, Removable a, HasNames a) => LiveVariables -> a -> LiveVariables
step live obj
    | isRemovable obj live = strip $ live
    | otherwise = strip $ stepSimple live obj
  where
    strip (LiveVariables live) = LiveVariables $ foldr Set.delete live (obj ^.. names)

stepSimple :: HasOperands a => LiveVariables -> a -> LiveVariables
stepSimple (LiveVariables live) obj = LiveVariables $ foldr Set.insert live (obj ^.. operands . operandPayload . _OperandNamed)

isLive :: HasNames a => a -> LiveVariables -> Bool
isLive obj (LiveVariables live) = obj & anyOf names (flip Set.member live)

class Removable a where
    isRemovable :: a -> LiveVariables -> Bool

    default isRemovable :: HasNames a => a -> LiveVariables -> Bool
    isRemovable obj live = not $ isLive obj live

instance Removable BlockEnd where
    isRemovable _ _ = False

instance Removable Instruction where
    isRemovable instr live = case instr ^. instrPayload of
        Call _ _ -> False
        Store _ _ _ -> False
        Inc _ _ -> False
        Dec _ _ -> False
        _ -> not $ any (flip isLive live) (instr ^. instrResult)

instance Removable PhiNode where

instance Pretty LiveVariables where
    pPrint (LiveVariables live) = foldl go empty live
      where
        go :: Doc -> Name -> Doc
        go acc var = acc $+$ pPrint var

instance Pretty (Map.Map Block LiveVariables) where
    pPrint m = ifoldl go empty m
      where
        go block acc vars =
            "live at the end of" <+> pPrint (block ^. blockName) $+$
                nest 4 (pPrint vars) $+$ acc


