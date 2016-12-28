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
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data MiddleEndState = MiddleEndState
    { _meNextUnique :: !Int
    , _meFunctions :: Map.Map Ident FunctionDescriptor
    , _meStrings :: Map.Map Ident BS.ByteString
    , _meObjects :: Map.Map Ident Object
    , _meDiagnostics :: Seq.Seq Diagnostic
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
report diagnostic = meDiagnostics %= (|> diagnostic)

addFunction :: (MonadState s m, HasMiddleEndState s) => Ident -> Block -> m ()
addFunction ident entryBlock = meFunctions . at ident ?= FunctionDescriptor entryBlock

internString :: (MonadState s m, HasMiddleEndState s) => Ident -> BS.ByteString -> m ()
internString name str = meStrings . at name ?= str

internObject :: (MonadState s m, HasMiddleEndState s) => Ident -> Object -> m ()
internObject name obj = meObjects . at name ?= obj

type Reportible r = (AST.HasLocRange r, Pretty r)

debugState :: MEMonad Doc
debugState = get >>= pPrintIO

run :: MEMonad () -> IO [Diagnostic]
run act = views meDiagnostics toList <$> execStateT act MiddleEndState
    { _meNextUnique = 0
    , _meFunctions = Map.empty
    , _meDiagnostics = Seq.empty
    , _meObjects = Map.empty
    , _meStrings = Map.empty
    }

instance PrettyIO FunctionDescriptor where
    pPrintIO desc = do
        blocks <- reachableBlocks $ desc ^. funcEntryBlock
        foldrM go empty blocks
      where
        go block doc = ($+$ doc) <$> pPrintIO block
    
instance PrettyIO MiddleEndState where
    pPrintIO state = do
       funcs <- ifoldrM goFunc empty (state ^. meFunctions)
       let strings = vcat ["string" <+> pPrint name <+> "=" <+> pPrint (BS.unpack str)
                          | (name, str) <- itoList $ state ^. meStrings]
       let objects = vcat [("object" <+> pPrint name) $+$ nest 4 (pPrint obj)
                          | (name, obj) <- itoList $ state ^. meObjects]
       pure $ funcs $+$ strings $+$ objects
      where
        goFunc name func acc = do
            body <- pPrintIO func
            pure $ ("function" <+> pPrint name) $+$ nest 4 body $+$ acc

instance Pretty Diagnostic where
    pPrint diag = hang main 4 (vcat . map pPrint $ diag ^. diagNotes)
      where
        header = sep
            [ pPrint $ diag ^. diagType
            , maybe empty pPrint $ diag ^. diagWhere
            ]
        main = header $+$ nest 4 (diag ^. diagContent)
        
instance Pretty DiagnosticType where
    pPrint DiagnosticError = "error"
    pPrint DiagnosticWarning = "warning"
    pPrint DiagnosticNote = "note"

