{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Backend.RegAlloc where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import qualified Language.Latte.Middleend.DeadCodeElimination as DCE
import qualified Language.Latte.Middleend.DataflowAnalysisEngine as DAE
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

newtype InterferenceGraph = InterferenceGraph { _getInterferenceGraph :: Map.Map Name (Set.Set Name) }

data MCSOrderState = MCSOrderState
    { _mcsStateLambda :: Map.Map Name Int
    , _mcsStateOrder :: Seq.Seq Name
    }

makeLenses ''InterferenceGraph
makeLenses ''MCSOrderState

allocateRegisters :: MonadIO m => [Block] -> m (Map.Map Name Int)
allocateRegisters blocks = do
    graph <- interferenceGraph blocks
    order <- mcsOrder blocks graph
    pure $ greedyColouring graph order

interferenceGraph :: MonadIO m => [Block] -> m InterferenceGraph
interferenceGraph blocks = liftIO . flip execStateT (InterferenceGraph Map.empty) $ do
    liveAtEnds <- DAE.runDAEBlocks blocks
    iforM_ (liveAtEnds :: Map.Map Block DCE.LiveVariables) $ \block liveAtEnd -> do 
        liveBeforeSourcePhi <- DAE.stepPhiSource liveAtEnd block
        end <- liftIO . readIORef $ block ^. blockEnd
        let liveBeforeEnd = DAE.stepEnd liveBeforeSourcePhi end

        body <- liftIO . readIORef $ block ^. blockBody
        liveBeforeBody <- foldrM updateInstruction liveBeforeEnd body
        
        phi <- liftIO . readIORef $ block ^. blockPhi
        phi & forMOf_ (folded . name) $ \first -> do
            phi & forMOf_ (folded . name) $ addConflict first
            addConflictsWithLive first liveBeforeBody
  where
    update :: (MonadState InterferenceGraph m, HasNames a)
        => (DCE.LiveVariables -> a -> DCE.LiveVariables)
        -> a -> DCE.LiveVariables -> m DCE.LiveVariables
    update step obj live = do
        let live' = step live obj
        addConflictsWithLive obj live'
        pure live'

    updateInstruction :: MonadState InterferenceGraph m
        => Instruction -> DCE.LiveVariables -> m DCE.LiveVariables
    updateInstruction instr live = do
        addConflictsWithLive instr live
        case instr ^. instrPayload of
            BinOp _ BinOpPlus rhs -> addConflictsWithOperands instr rhs
            BinOp _ BinOpMinus rhs -> addConflictsWithOperands instr rhs
            BinOp _ BinOpTimes rhs -> addConflictsWithOperands instr rhs
            BinOp _ BinOpShiftLeft rhs -> addConflictsWithOperands instr rhs
            BinOp _ BinOpShiftRight rhs -> addConflictsWithOperands instr rhs
            BinOp _ BinOpAnd rhs -> addConflictsWithOperands instr rhs
            BinOp _ BinOpOr rhs -> addConflictsWithOperands instr rhs
            BinOp _ BinOpDivide _ -> pure ()
            BinOp _ BinOpModulo _ -> pure ()
            BinOp _ BinOpLess _ -> pure ()
            BinOp _ BinOpLessEqual _ -> pure ()
            BinOp _ BinOpGreater _ -> pure ()
            BinOp _ BinOpGreaterEqual _ -> pure ()
            BinOp _ BinOpEqual _ -> pure ()
            BinOp _ BinOpNotEqual _ -> pure ()
            UnOp UnOpNeg _ -> pure ()
            UnOp UnOpNot _ -> pure ()
            Store _ _ _ -> pure ()
            Load _ _ -> pure ()
            GetAddr _ -> pure ()
            Inc _ _ -> pure ()
            Dec _ _ -> pure ()
            IConst _ -> pure ()
            IIntristic _ -> pure ()
            Call _ _ -> pure ()
        pure $ DAE.stepInstruction live instr
    
    addDirected a b = getInterferenceGraph . at a . non Set.empty . contains b .= True

    addConflict a b | a == b = pure ()
    addConflict a b = addDirected a b >> addDirected b a

    addConflictsWithLive obj (DCE.LiveVariables live) = obj & forMOf_ names $ forM_ live . addConflict
    addConflictsWithOperands obj x = obj & forMOf_ names $ forMOf_ (operands . operandPayload . _OperandNamed) x . addConflict

mcsOrder :: MonadIO m => [Block] -> InterferenceGraph -> m [Name]
mcsOrder blocks graph = do
    names <- Set.unions <$> mapM getNames blocks
    st <- execStateT (replicateM (length names) phase) MCSOrderState
        { _mcsStateLambda = Map.fromList [(name, 0) | name <- Set.toList names]
        , _mcsStateOrder = Seq.empty
        }
    pure . toList $ st ^. mcsStateOrder
  where
    getNames block = do
        phi <- liftIO . readIORef $ block ^. blockPhi
        body <- liftIO . readIORef $ block ^. blockBody
        pure $ Set.union (namesOf phi) (namesOf body)

    namesOf xs = Set.fromList $ xs ^.. traverse . names

    phase = do
        picked <- uses mcsStateLambda pick
        mcsStateOrder %= flip snoc picked
        forM_ (graph ^. getInterferenceGraph . at picked . non Set.empty) $ \neighbour ->
            mcsStateLambda . at neighbour . traverse += 1
        mcsStateLambda . at picked .= Nothing

    pick lambda = fst . maximumBy (compare `on` snd) $ Map.toList lambda

greedyColouring :: InterferenceGraph -> [Name] -> Map.Map Name Int
greedyColouring graph = foldl go Map.empty
  where
    go acc name = Map.insert name colour acc
      where
        neighbours = graph ^. getInterferenceGraph . at name . non Set.empty
        neighboursColours = Set.map (`Map.lookup` acc) neighbours
        colour = head [i | i <- [0..], not $ Just i `Set.member` neighboursColours]

instance Pretty InterferenceGraph where
    pPrint (InterferenceGraph graph) = foldr ($+$) empty $ map go $ Map.toList graph
      where
        go (name, others) = pPrint name <+> " conflicts with: " <+> hsep (map pPrint $ toList others)
