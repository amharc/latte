{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.DeadCodeElimination (opt) where

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

newtype LiveVariables = LiveVariables (Set.Set Name)
    deriving (Eq, Show)

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = do
    liveMap <- DAE.runDAE
    liftIO . putStrLn . render $ pPrint liveMap
    use meFunctions >>= mapM_ (runFunction liveMap)

runFunction :: MonadIO m => Map.Map Block LiveVariables -> FunctionDescriptor -> m ()
runFunction live func = reachableBlocks (func ^. funcEntryBlock) >>= traverse_ (runBlock live)

runBlock :: MonadIO m => Map.Map Block LiveVariables -> Block -> m ()
runBlock liveMap block = liftIO $ do
    phi <- readIORef $ block ^. blockPhi
    body <- readIORef $ block ^. blockBody
    end <- readIORef $ block ^. blockEnd

    let (body', liveBeforeBody) = foldr go (Seq.empty, step liveAfter end) body
        (phi', _) = foldr go (Seq.empty, liveBeforeBody) phi

    writeIORef (block ^. blockPhi) phi'
    writeIORef (block ^. blockBody) body'
  where
    liveAfter = liveMap Map.! block

    go :: (HasOperands a, Removable a) => a -> (Seq.Seq a, LiveVariables) -> (Seq.Seq a, LiveVariables)
    go item (acc, live)
        | isRemovable item live = (acc, live)
        | otherwise = (item Seq.<| acc, step live item)

instance DAE.DAE LiveVariables where
    direction = DAE.DAEBackward
    postProcess = DAE.DAEMerge
    stepInstruction = step
    stepPhi = step
    stepEnd = step

instance Monoid LiveVariables where
    mempty = LiveVariables Set.empty
    LiveVariables lhs `mappend` LiveVariables rhs = LiveVariables $ Set.union lhs rhs

step :: (HasOperands a, Removable a) => LiveVariables -> a -> LiveVariables
step live obj
    | isRemovable obj live = live
    | otherwise = stepSimple live obj
  where
    stepSimple (LiveVariables live) obj = LiveVariables $ foldr Set.insert live (obj ^.. operands . _OperandNamed)

isLive :: HasName a => a -> LiveVariables -> Bool
isLive obj (LiveVariables live) = Set.member (obj ^. name) live

class Removable a where
    isRemovable :: a -> LiveVariables -> Bool

    default isRemovable :: HasName a => a -> LiveVariables -> Bool
    isRemovable obj live = not $ isLive obj live

instance Removable BlockEnd where
    isRemovable _ _ = False

instance Removable Instruction where
    isRemovable instr live = case instr ^. instrPayload of
        Call _ _ -> False
        Store _ _ _ -> False
        Inc _ _ -> False
        Dec _ _ -> False
        _ -> not $ isLive instr live

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

