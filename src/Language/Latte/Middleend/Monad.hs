{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Latte.Middleend.Monad where

import Language.Latte.Middleend.IR
import qualified Language.Latte.Frontend.AST as AST
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data MiddleEndState = MiddleEndState
    { _meNextUnique :: !Int
    , _meBlocks :: Map.Map Name Block
    , _meDiagnostics :: [Diagnostic]
    }

data Diagnostic = Diagnostic
    { _diagWhere :: !(Maybe AST.LocRange)
    , _diagContent :: !Doc
    , _diagNotes :: [Diagnostic]
    , _diagType :: !DiagnosticType
    }

data DiagnosticType = DiagnosticError | DiagnosticWarning | DiagnosticNote

makeClassy ''MiddleEndState
makeLenses ''Diagnostic

type MEMonad m = MonadState MiddleEndState m

nextUniqueId :: (MonadState s m, HasMiddleEndState s) => m UniqueId
nextUniqueId = fmap UniqueId (meNextUnique <+= 1)

mkName :: (MonadState s m, HasMiddleEndState s) => Maybe Ident -> m Name
mkName human = flip Name human <$> nextUniqueId

report :: (MonadState s m, HasMiddleEndState s) => Diagnostic -> m ()
report diagnostic = meDiagnostics %= cons diagnostic

addBlock ::(MonadState s m, HasMiddleEndState s) => Block -> m ()
addBlock blk =
    use (meBlocks . at (blk ^. name)) >>= \case
        Nothing -> meBlocks . at (blk ^. name) ?= blk
        Just _ -> fail "Block redefinition"

type Reportible r = (AST.HasLocRange r, Pretty r)
