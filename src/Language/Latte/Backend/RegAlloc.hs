{-# LANGUAGE FlexibleContexts #-}
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
        end <- liftIO . readIORef $ block ^. blockEnd
        let liveBeforeEnd = DAE.stepEnd liveAtEnd end

        body <- liftIO . readIORef $ block ^. blockBody
        liveBeforeBody <- foldrM (update DAE.stepInstruction) liveBeforeEnd body
        
        phi <- liftIO . readIORef $ block ^. blockPhi
        foldrM (update DAE.stepPhi) liveBeforeBody phi
  where
    update :: (MonadState InterferenceGraph m, HasNames a)
        => (DCE.LiveVariables -> a -> DCE.LiveVariables)
        -> a -> DCE.LiveVariables -> m DCE.LiveVariables
    update step obj live = do
        let live'@(DCE.LiveVariables liveBefore) = step live obj
        obj & forMOf_ names $ \name ->
            forM_ liveBefore $ \other -> do
                addDirected name other
                addDirected other name
        pure $ live'
    
    addDirected a b = getInterferenceGraph . at a . non Set.empty . contains b .= True

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
