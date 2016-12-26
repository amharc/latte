{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Latte.Frontend.AST where

import Control.Lens
import qualified Data.ByteString.Char8 as BS

data Location = Location
  { _locLine :: {-# UNPACK #-} !Int
  , _locColumn :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

data LocRange = LocRange
  { _locStart :: {-# UNPACK #-} !Location
  , _locEnd :: {-# UNPACK #-} !Location
  }
  deriving (Eq, Show)

data Located a = Located
  { _loc :: {-# UNPACK #-} !LocRange
  , _obj :: !a
  }
  deriving (Eq, Show, Functor)

newtype Program = Program { getProgram :: [Located TopLevelDecl] }
  deriving Show

data TopLevelDecl
  = TLDFunc FuncDecl
  | TLDClass ClassDecl
  deriving Show

newtype Ident = Ident { getIdent :: BS.ByteString }
  deriving Show

data FuncDecl = FuncDecl
  { _funcName :: !Ident
  , _funcArgs :: [FunArg]
  , _funcRetType :: !Type
  , _funcBody :: !Stmt
  }
  deriving Show

data Type
  = TyInt
  | TyBool
  | TyVoid
  | TyArray !Type
  | TyClass !BS.ByteString
  deriving Show

data Stmt
  = StmtBlock [Located Stmt]
  | StmtReturn !(Maybe Expr)
  | StmtInc !Lval
  | StmtDec !Lval
  | StmtIf !(Located Expr) !(Located Stmt) !(Located Stmt)
  | StmtWhile !(Located Expr) !(Located Stmt)
  | StmtExpr !Expr
  | StmtDecl !LocalDecl
  deriving Show

data Expr
  = ExprLval !Lval
  | ExprInt !Int
  | ExprString !BS.ByteString
  | ExprTrue
  | ExprFalse
  | ExprNull
  | ExprCall !Ident [Located Expr]
  | ExprUnOp !UnOp !(Located Expr)
  | ExprBinOp !(Located Expr) !BinOp !(Located Expr)
  | ExprNew !Ident
  | ExprNewArr !Ident !(Located Expr)
  deriving Show

data UnOp
  = UnOpNeg
  | UnOpNot
  deriving Show

data BinOp
  = BinOpPlus
  | BinOpMinus
  | BinOpTimes
  | BinOpDivide
  | BinOpModulo
  | BinOpLess
  | BinOpLessEqual
  | BinOpGreater
  | BinOpGreaterEqual
  | BinOpEqual
  | BinOpNotEqual
  deriving Show

data LocalDecl = LocalDecl
  { _localDeclType :: !Type
  , _localDeclItems :: [LocalDeclItem]
  }
  deriving Show

data LocalDeclItem = LocalDeclItem
  { _localDeclName :: !Ident
  , _localDeclValue :: !Expr
  }
  deriving Show

data Lval
  = LvalVar !Ident
  | LvalArray !(Located Lval) !(Located Expr)
  deriving Show

data FunArg = FunArg
  { _funArgType :: !Type
  , _funArgName :: !Ident
  }
  deriving Show

data ClassDecl = ClassDecl
  { _className :: !Ident
  , _classBase :: !(Maybe Ident)
  , _classFields :: [Located ClassField]
  , _classMethods :: [Located FuncDecl]
  }
  deriving Show

data ClassField = ClassField
  { _classFieldType :: !Type
  , _classFieldName :: !Ident
  }
  deriving Show

pattern TyString = TyClass "string"
pattern StmtEmpty = StmtBlock []

makeLenses ''Location
makeClassy ''LocRange
makeLenses ''Located
makeLenses ''FuncDecl
makeLenses ''LocalDecl
makeLenses ''LocalDeclItem
makeLenses ''FunArg
makeLenses ''ClassDecl
makeLenses ''ClassField
