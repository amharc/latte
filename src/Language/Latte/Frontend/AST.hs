{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.Latte.Frontend.AST where

import Control.Lens
import qualified Data.Semigroup as Semi
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Location = Location
    { _locLine :: {-# UNPACK #-} !Int
    , _locColumn :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Show, Ord)

data LocRange = LocRange
    { _locStart :: {-# UNPACK #-} !Location
    , _locEnd :: {-# UNPACK #-} !Location
    }
    deriving (Eq, Show, Ord)

data Located a = Located
    { _loc :: {-# UNPACK #-} !LocRange
    , _obj :: !a
    }
    deriving (Eq, Show, Functor)

pattern Loc :: a -> Located a
pattern Loc obj <- Located _ obj

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
    | TyString
    | TyArray !Type
    | TyClass !Ident
    deriving Show

data Stmt
    = StmtBlock [Located Stmt]
    | StmtReturn !(Maybe Expr)
    | StmtInc !Lval
    | StmtDec !Lval
    | StmtIf !(Located Expr) !(Located Stmt) !(Maybe (Located Stmt))
    | StmtWhile !(Located Expr) !(Located Stmt)
    | StmtFor !Type !Ident !(Located Expr) !(Located Stmt)
    | StmtAssign !Lval !(Located Expr)
    | StmtExpr !Expr
    | StmtDecl !LocalDecl
    | StmtNone
    deriving Show

data Expr
    = ExprLval !Lval
    | ExprInt !Int
    | ExprString !BS.ByteString
    | ExprTrue
    | ExprFalse
    | ExprNull
    | ExprCall !Lval [Located Expr]
    | ExprUnOp !UnOp !(Located Expr)
    | ExprBinOp !(Located Expr) !BinOp !(Located Expr)
    | ExprNew !Type
    | ExprNewArr !Type !(Located Expr)
    | ExprCast !Type !(Located Expr)
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
    | BinOpAnd
    | BinOpOr
    deriving Show

data LocalDecl = LocalDecl
    { _localDeclType :: !Type
    , _localDeclItems :: [Located LocalDeclItem]
    }
    deriving Show

data LocalDeclItem = LocalDeclItem
    { _localDeclName :: !Ident
    , _localDeclValue :: !(Maybe (Located Expr))
    }
    deriving Show

data Lval
    = LvalVar !Ident
    | LvalArray !(Located Expr) !(Located Expr)
    | LvalField !(Located Expr) !Ident
    deriving Show

data FunArg = FunArg
    { _funArgType :: !Type
    , _funArgName :: !Ident
    }
    deriving Show

data ClassDecl = ClassDecl
    { _className :: !Ident
    , _classBase :: !(Maybe Ident)
    , _classMembers :: [Located ClassMember]
    }
    deriving Show

data ClassMember
    = ClassMemberField !ClassField
    | ClassMemberMethod !FuncDecl
    deriving Show

data ClassField = ClassField
    { _classFieldType :: !Type
    , _classFieldName :: !Ident
    }
    deriving Show

makeLenses ''Location
makeClassy ''LocRange
makeLenses ''Located
makeLenses ''FuncDecl
makeLenses ''LocalDecl
makeLenses ''LocalDeclItem
makeLenses ''FunArg
makeLenses ''ClassDecl
makeLenses ''ClassField

instance Semi.Semigroup LocRange where
    lhs <> rhs = LocRange
        (min (lhs ^. locStart) (rhs ^. locStart))
        (max (lhs ^. locStart) (rhs ^. locStart))

instance Pretty Ident where
    pPrint (Ident ident) = text (BS.unpack ident)

instance Pretty Location where
    pPrint l = pPrint (l ^. locLine) <> ":" <> pPrint (l ^. locColumn)

instance Pretty LocRange where
    pPrint l = pPrint (l ^. locStart) <> "-" <> pPrint (l ^. locEnd)

instance Pretty Program where
    pPrint (Program decls) = vcat $ map (views obj pPrint) decls

instance Pretty TopLevelDecl where
    pPrint (TLDFunc decl) = pPrint decl
    pPrint (TLDClass decl) = pPrint decl

instance Pretty FuncDecl where
    pPrint decl = blockedStmts
        (pPrint (decl ^. funcRetType) 
          <+> pPrint (decl ^. funcName)
          <>  parens (sep . punctuate comma $ map pPrint (decl ^. funcArgs)))
        (view funcBody decl)

instance Pretty Type where
    pPrint TyInt = "int"
    pPrint TyBool = "boolean"
    pPrint TyVoid = "void"
    pPrint TyString = "string"
    pPrint (TyArray ty) = pPrint ty <> "[]"
    pPrint (TyClass ident) = pPrint ident

instance Pretty Stmt where
    pPrint (StmtBlock stmts) = blocked empty (vcat $ map (views obj pPrint) stmts)
    pPrint (StmtReturn Nothing) = "return;"
    pPrint (StmtReturn (Just expr)) = "return" <+> pPrint expr <> semi
    pPrint (StmtInc lval) = pPrint lval <> "++" <> semi
    pPrint (StmtDec lval) = pPrint lval <> "--" <> semi
    pPrint (StmtIf (Loc cond) (Loc ifTrue) mIfFalse) = ifBranch $+$ elseBranch
      where
        ifBranch = blockedStmts ("if" <+> parens (pPrint cond)) ifTrue
        elseBranch = case mIfFalse of
            Nothing -> empty
            Just (Loc ifFalse) -> blockedStmts ("else") ifFalse
    pPrint (StmtWhile (Loc cond) (Loc body)) = blockedStmts
        ("while" <+> parens (pPrint cond))
        body
    pPrint (StmtFor ty name (Loc expr) (Loc body)) = blockedStmts
        ("for" <+> parens (pPrint ty <+> pPrint name <+> colon <+> pPrint expr))
        body
    pPrint (StmtAssign lval (Loc expr)) = pPrint lval <+> equals <+> pPrint expr <> semi
    pPrint (StmtExpr expr) = pPrint expr <> semi
    pPrint (StmtDecl decl) = pPrint decl <> semi
    pPrint StmtNone = semi

instance Pretty Expr where
    pPrintPrec _ _ (ExprLval lval) = pPrint lval
    pPrintPrec _ _ (ExprInt i) = int i
    pPrintPrec _ _ (ExprString str) = doubleQuotes . text $ BS.unpack str
    pPrintPrec _ _ ExprTrue = "true"
    pPrintPrec _ _ ExprFalse = "false"
    pPrintPrec _ _ ExprNull = "null"
    pPrintPrec _ _ (ExprCast ty (Loc ex)) = parens (pPrint ty) <+> pPrint ex
    pPrintPrec _ _ (ExprNew ty) = "new" <+> pPrint ty
    pPrintPrec _ _ (ExprNewArr ty (Loc expr)) = "new" <+> pPrint ty <> brackets (pPrint expr)
    pPrintPrec _ _ (ExprCall ident args) = pPrint ident 
        <> parens (cat . punctuate comma $ map (views obj pPrint) args)
    pPrintPrec _ _ (ExprUnOp oper (Loc expr)) = op' <> pPrint expr
        where
            op' = case oper of
                UnOpNeg -> "-"
                UnOpNot -> "!"
    pPrintPrec l r (ExprBinOp (Loc lhs) oper (Loc rhs)) = maybeParens (r >= precLeft) $ sep
        [ pPrintPrec l precLeft lhs
        , op'
        , pPrintPrec l precRight rhs
        ]
      where
        (precLeft, op', precRight) = case oper of
            BinOpPlus -> (addPrec, "+", addPrec - eps)
            BinOpMinus -> (addPrec, "-", addPrec - eps)
            BinOpTimes -> (mulPrec, "*", mulPrec - eps)
            BinOpDivide -> (mulPrec, "/", mulPrec - eps)
            BinOpModulo -> (mulPrec, "%", mulPrec - eps)
            BinOpLess -> (relPrec, "<", relPrec)
            BinOpLessEqual -> (relPrec, "<=", relPrec)
            BinOpGreater -> (relPrec, ">", relPrec)
            BinOpGreaterEqual -> (relPrec, ">=", relPrec)
            BinOpEqual -> (relPrec, "==", relPrec)
            BinOpNotEqual -> (relPrec, "!=", relPrec)
            BinOpAnd -> (logPrec, "&&", logPrec - eps)
            BinOpOr -> (logPrec, "||", logPrec - eps)

        eps = 1 / 10000

        addPrec = 5
        mulPrec = 4
        relPrec = 6
        logPrec = 7

instance Pretty FunArg where
    pPrint f = pPrint (f ^. funArgType) <+> pPrint (f ^. funArgName)

instance Pretty Lval where
    pPrintPrec _ _ (LvalVar ident) = pPrint ident
    pPrintPrec l _ (LvalArray (Loc arr) (Loc idx)) =
        pPrintPrec l 8 arr <> brackets (pPrint idx)
    pPrintPrec l _ (LvalField (Loc object) field) =
        pPrintPrec l 8 object <> "." <> pPrint field

instance Pretty LocalDecl where
    pPrint decl = pPrint (decl ^. localDeclType) 
        <+> (sep . punctuate comma $ map (views obj pPrint) (decl ^. localDeclItems))

instance Pretty LocalDeclItem where
    pPrint (LocalDeclItem name Nothing) = pPrint name
    pPrint (LocalDeclItem name (Just (Loc initializer))) =
        pPrint name <+> equals <+> pPrint initializer

instance Pretty ClassDecl where
    pPrint (ClassDecl name base members) = blocked 
        (sep [ "class"
             , pPrint name
             , case base of
                Nothing -> empty
                Just b -> "extends" <+> pPrint b
             ])
        (vcat $ map (views obj pPrint) members)

instance Pretty ClassMember where
    pPrint (ClassMemberField field) = pPrint field
    pPrint (ClassMemberMethod method) = pPrint method

instance Pretty ClassField where
    pPrint (ClassField ty name) = pPrint ty <+> pPrint name <> semi

pretty :: Pretty a => a -> String
pretty = render . pPrint

blocked :: Doc -> Doc -> Doc
blocked header body = (header <+> lbrace) $+$ nest 4 body $+$ rbrace

blockedStmts :: Doc -> Stmt -> Doc
blockedStmts header (StmtBlock stmts) = blocked header (vcat $ views obj pPrint <$> stmts)
blockedStmts header stmt = blocked header (pPrint stmt)
