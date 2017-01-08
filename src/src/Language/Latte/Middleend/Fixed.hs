module Language.Latte.Middleend.Fixed (iterOpt) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

type Snapshot = Map.Map Name (Seq.Seq PhiNode, Seq.Seq Instruction, BlockEnd)

iterOpt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => Int -> m () -> m ()
iterOpt remaining act = do
    s <- snapshot
    go remaining s
  where
    go 0 _ = pure ()
    go n s = do
        act
        s' <- snapshot
        when (s /= s') $ go (n - 1) s'

snapshot :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m Snapshot
snapshot = do
    functions <- uses meFunctions . toListOf $ traverse . funcEntryBlock
    blocks <- concat <$> mapM reachableBlocks functions
    foldM add Map.empty blocks
  where
    add s block = liftIO $ do
        phi <- readIORef $ block ^. blockPhi
        body <- readIORef $ block ^. blockBody
        end <- readIORef $ block ^. blockEnd
        pure $ Map.insert (block ^. blockName) (phi, body, end) s
