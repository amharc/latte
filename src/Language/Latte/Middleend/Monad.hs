{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.Monad where

import Language.Latte.Middleend.IR
import qualified Language.Latte.Frontend.AST as AST
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data MiddleEndState = MiddleEndState
    { _meNextUnique :: !Int
    , _meFunctions :: Map.Map Ident FunctionDescriptor
    , _meDiagnostics :: [Diagnostic]
    }

data Diagnostic = Diagnostic
    { _diagWhere :: !(Maybe AST.LocRange)
    , _diagContent :: !Doc
    , _diagNotes :: [Diagnostic]
    , _diagType :: !DiagnosticType
    }

data FunctionDescriptor = FunctionDescriptor
    { _funcEntryBlock :: Block
    }

data DiagnosticType = DiagnosticError | DiagnosticWarning | DiagnosticNote

makeClassy ''MiddleEndState
makeLenses ''Diagnostic
makeLenses ''FunctionDescriptor

type MEMonad a = StateT MiddleEndState IO a

nextUniqueId :: (MonadState s m, HasMiddleEndState s) => m UniqueId
nextUniqueId = fmap UniqueId (meNextUnique <+= 1)

mkName :: (MonadState s m, HasMiddleEndState s) => Maybe Ident -> m Name
mkName human = flip Name human <$> nextUniqueId

report :: (MonadState s m, HasMiddleEndState s) => Diagnostic -> m ()
report diagnostic = meDiagnostics %= cons diagnostic

addFunction :: (MonadState s m, HasMiddleEndState s) => Ident -> Block -> m ()
addFunction ident entryBlock = meFunctions . at ident ?= FunctionDescriptor entryBlock

type Reportible r = (AST.HasLocRange r, Pretty r)

debugState :: MEMonad Doc
debugState = do
    funcs <- use meFunctions
    ifoldrM go empty funcs
  where
    go :: Ident -> FunctionDescriptor -> Doc -> MEMonad Doc
    go name func acc = do
        body <- pPrintIO func
        pure $ hang ("function" <+> pPrint name) 4 body $+$ acc

run :: MEMonad () -> IO [Diagnostic]
run act = view meDiagnostics <$> execStateT act MiddleEndState
    { _meNextUnique = 0
    , _meFunctions = Map.empty
    , _meDiagnostics = []
    }

instance PrettyIO FunctionDescriptor where
    pPrintIO desc = go (Seq.singleton $ desc ^. funcEntryBlock) Set.empty
      where
        go :: MonadIO m => Seq.Seq Block -> Set.Set Name -> m Doc
        go queue visited = case Seq.viewl queue of
            Seq.EmptyL -> pure empty
            block Seq.:< blocks -> do
                current <- pPrintIO block
                succs <- successors block
                let (queue, set) = foldl add (blocks, visited) succs
                (current $+$) <$> go queue set

        add (queue, set) block
            | Set.member (block ^. blockName) set = (queue, set)
            | otherwise = (queue |> block, Set.insert (block ^. blockName) set)

instance Pretty Diagnostic where
    pPrint diag = hang header 4 (vcat . map pPrint $ diag ^. diagNotes)
      where
        header = vcat
            [ pPrint $ diag ^. diagType
            , maybe empty pPrint $ diag ^. diagWhere
            , ":"
            , diag ^. diagContent
            ]
        
instance Pretty DiagnosticType where
    pPrint DiagnosticError = "error"
    pPrint DiagnosticWarning = "warning"
    pPrint DiagnosticNote = "note"
