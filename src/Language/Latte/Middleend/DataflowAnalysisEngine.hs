{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.DataflowAnalysisEngine (DAEDirection(..), DAEPostProcess(..), DAE(..), runDAE) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens
import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data DAEDirection = DAEForward | DAEBackward

data DAEPostProcess = DAEMerge | DAENormal

class (Monoid a, Eq a, Pretty a) => DAE a where
    direction :: DAEDirection

    postProcess :: DAEPostProcess
    postProcess = DAENormal

    stepInstruction :: a -> Instruction -> a
    stepPhi :: a -> PhiNode -> a
    stepEnd :: a -> BlockEnd -> a

runDAE :: (MonadIO m, MonadState s m, HasMiddleEndState s, DAE a) => m (Map.Map Block a)
runDAE = uses meFunctions toList >>= fmap Map.unions . traverse function
  where
    function desc = reachableBlocks (desc ^. funcEntryBlock) >>= run'

run' :: forall m a. (MonadIO m, DAE a) => [Block] -> m (Map.Map Block a)
run' blocks = preds >>= go (Map.fromList [(block, mempty) | block <- blocks])
  where
    go prev preds = do
        cur <- iteration preds order prev
        if prev == cur
        then
            pure $ post preds cur
        else
            go cur preds

    order = case direction @a of
        DAEForward -> blocks
        DAEBackward -> reverse blocks

    preds = case direction @a of
        DAEForward -> predecessors blocks 
        DAEBackward -> Map.fromList <$> forM blocks (\block -> (block,) <$> successors block)

iteration :: forall m a. (MonadIO m, DAE a) => Map.Map Block [Block] -> [Block] -> Map.Map Block a -> m (Map.Map Block a)
iteration preds order prev = liftIO $ foldlM addBlock prev order
  where
    addBlock prev block = do
        value <- stepBlock prev block
        pure $ Map.insert block value prev

    stepBlock prev block = do
        putStrLn . render $ "preds of " <+> pPrint block <> ":" <+> pPrintList prettyNormal (preds Map.! block)
        phis <- readIORef $ block ^. blockPhi
        body <- readIORef $ block ^. blockBody
        end <- readIORef $ block ^. blockEnd

        let init = foldMap (prev Map.!) (preds Map.! block)

        putStrLn . render $ "init of" <+> pPrint block <> ":" <+> pPrint init

        let ret = case direction @a of
                DAEForward ->
                    init & flip (foldl stepPhi) phis
                         & flip (foldl stepInstruction) body
                         & flip stepEnd end
                DAEBackward -> 
                    init & flip stepEnd end
                         & flip (foldr (flip stepInstruction)) body
                         & flip (foldr (flip stepPhi)) phis

        putStrLn . render $ "result of" <+> pPrint block <> ":" <+> pPrint ret
        pure ret

post :: forall a. DAE a => Map.Map Block [Block] -> Map.Map Block a -> Map.Map Block a
post preds prev = case postProcess @a of
    DAENormal -> prev
    DAEMerge -> imap (\block _ -> foldMap (prev Map.!) (preds Map.! block)) prev
