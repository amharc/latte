{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
#endif
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.DataflowAnalysisEngine (DAEDirection(..), DAEPostProcess(..), DAE(..), runDAE, runDAEBlocks, stepPhiSource) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens
import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
#if __GLASGOW_HASKELL__ < 801
import Data.Proxy
#endif
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

data DAEDirection = DAEForward | DAEBackward

data DAEPostProcess = DAEMerge | DAENormal

class (Monoid a, Eq a) => DAE a where
#if __GLASGOW_HASKELL__ >= 801
    direction :: DAEDirection

    postProcess :: DAEPostProcess
    postProcess = DAENormal
#else
    direction :: Proxy a -> DAEDirection

    postProcess :: Proxy a -> DAEPostProcess
    postProcess _ = DAENormal
#endif

    stepInstruction :: a -> Instruction -> a
    stepPhiBranch :: a -> Operand -> a
    stepPhiTarget :: Foldable f => a -> f PhiNode -> a
    stepEnd :: a -> BlockEnd -> a

stepPhiSource :: (DAE a, MonadIO m) => a -> Block -> m a
stepPhiSource dae block = do
    succsPhi <- successors block >>= mapM (views blockPhi $ liftIO . readIORef)
    pure $ foldl stepPhiBranch dae $
        succsPhi ^.. folded . folded . phiBranches . folded . filtered (has $ phiFrom . only block) . phiValue

runDAE :: (MonadIO m, MonadState s m, HasMiddleEndState s, DAE a) => m (Map.Map Block a)
runDAE = uses meFunctions toList >>= fmap Map.unions . traverse function
  where
    function desc = reachableBlocks (desc ^. funcEntryBlock) >>= runDAEBlocks

runDAEBlocks :: forall m a . (MonadIO m, DAE a) => [Block] -> m (Map.Map Block a)
runDAEBlocks blocks = preds >>= go (Map.fromList [(block, mempty) | block <- blocks])
  where
    go prev preds = do
        cur <- iteration preds order prev
        if prev == cur
        then
            pure $ post preds cur
        else
            go cur preds

#if __GLASGOW_HASKELL__ >= 801
    order = case direction @a of
#else
    order = case direction (Proxy :: Proxy a) of
#endif
        DAEForward -> blocks
        DAEBackward -> reverse blocks

#if __GLASGOW_HASKELL__ >= 801
    preds = case direction @a of
#else
    preds = case direction (Proxy :: Proxy a) of
#endif
        DAEForward -> predecessors blocks 
        DAEBackward -> Map.fromList <$> forM blocks (\block -> (block,) <$> successors block)

iteration :: forall m a. (MonadIO m, DAE a) => Map.Map Block [Block] -> [Block] -> Map.Map Block a -> m (Map.Map Block a)
iteration preds order prev = liftIO $ foldlM addBlock prev order
  where
    addBlock prev block = do
        value <- stepBlock prev block
        pure $ Map.insert block value prev

    stepBlock prev block = do
        phis <- readIORef $ block ^. blockPhi
        body <- readIORef $ block ^. blockBody
        end <- readIORef $ block ^. blockEnd

        let init = foldMap (prev Map.!) (preds Map.! block)

#if __GLASGOW_HASKELL__ >= 801
        case direction @a of
#else
        case direction (Proxy :: Proxy a) of
#endif
            DAEForward -> do
                let first = init & flip stepPhiTarget phis
                                 & flip (foldl stepInstruction) body
                                 & flip stepEnd end
                stepPhiSource first block
            DAEBackward -> do
                first <- stepPhiSource init block
                pure $ first & flip stepEnd end
                             & flip (foldr (flip stepInstruction)) body
                             & flip stepPhiTarget phis

post :: forall a. DAE a => Map.Map Block [Block] -> Map.Map Block a -> Map.Map Block a
#if __GLASGOW_HASKELL__ >= 801
post preds prev = case postProcess @a of
#else
post preds prev = case postProcess (Proxy :: Proxy a) of
#endif
    DAENormal -> prev
    DAEMerge -> imap (\block _ -> foldMap (prev Map.!) (preds Map.! block)) prev
