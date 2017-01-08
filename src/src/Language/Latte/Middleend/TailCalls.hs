{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.TailCalls(opt) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IORef
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = use meFunctions >>= imapM_ runFunction

runFunction :: MonadIO m => Ident -> FunctionDescriptor -> m ()
runFunction name func = reachableBlocks (func ^. funcEntryBlock) >>= mapM_ (runBlock name)

runBlock :: MonadIO m => Ident -> Block -> m ()
runBlock name block = liftIO $ do
    end <- readIORef $ block ^. blockEnd
    body <- readIORef $ block ^. blockBody
    case (Seq.viewr body, end) of
        (instrs Seq.:> instr@(view instrPayload -> Call dest args), BlockEndReturnVoid) -> do
            writeIORef (block ^. blockBody) instrs
            writeIORef (block ^. blockEnd) . BlockEndTailCall $ SCall dest args

        (instrs Seq.:> instr@(view instrPayload -> Call dest args), BlockEndReturn (Operand (OperandNamed ret) _))
            | Just ret == instr ^. instrResult -> do
                writeIORef (block ^. blockBody) instrs
                writeIORef (block ^. blockEnd) . BlockEndTailCall $ SCall dest args

        _ -> pure ()
