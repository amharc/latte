{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.CheckUnreachability (check) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IORef
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad
import Text.PrettyPrint.HughesPJClass

check :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
check = use meFunctions >>= imapM_ runFunction

runFunction :: (MonadIO m, MonadState s m, HasMiddleEndState s) => Ident -> FunctionDescriptor -> m ()
runFunction name func = reachableBlocks (func ^. funcEntryBlock) >>= mapM_ (runBlock name)

runBlock :: (MonadIO m, MonadState s m, HasMiddleEndState s) => Ident -> Block -> m ()
runBlock name block = liftIO (readIORef $ block ^. blockEnd) >>= \case
    BlockEndNone -> report Diagnostic
        { _diagWhere = Nothing
        , _diagContent = "Control reached the end of non-void function " <+> pPrint name
        , _diagNotes = []
        , _diagType = DiagnosticError
        }
    _ -> pure ()
