{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    { _diagWhere :: !(Maybe AST.Location)
    , _diagContent :: !Doc
    , _diagNotes :: [Diagnostic]
    , _diagType :: !DiagnosticType
    }

data DiagnosticType = DiagnosticError | DiagnosticWarning | DiagnosticNote

makeLenses ''MiddleEndState
makeLenses ''Diagnostic

type MEMonad m = MonadState MiddleEndState m

nextUniqueId :: MEMonad m => m UniqueId
nextUniqueId = fmap UniqueId (meNextUnique <+= 1)

mkName :: MEMonad m => Maybe Ident -> m Name
mkName human = flip Name human <$> nextUniqueId

report :: MEMonad m => Diagnostic -> m ()
report diagnostic = meDiagnostics %= cons diagnostic

addBlock :: MEMonad m => Block -> m ()
addBlock blk =
    view (meBlocks . at (blk ^. name)) >>= \case
        Nothing -> meBlocks . at (blk ^. name) ?= blk
        Just x -> fail "Block redefinition"

type Reportible r = (AST.HasLocation r, Pretty r)
