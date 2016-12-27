{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Language.Latte.Middleend.GenIR where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Language.Latte.Frontend.AST as AST
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data GIRState = GIRState
    { _girME :: !MiddleEndState
    , _girVariableCnt :: !Int
    , _girVariables :: Map.Map AST.Ident Variable
    , _girCurrentBlock :: Block
    }

data GIREnv = GIREnv
    { _girCurrentFunction :: Maybe AST.FuncDecl
    , _girCurrentClass :: Maybe AST.ClassDecl
    , _girFunctions :: Map.Map AST.Ident FuncInfo
    , _girClasses :: Map.Map AST.Ident ClassInfo
    }

data FuncInfo = FuncInfo
    { _funcInfoDecl :: AST.FuncDecl
    , _funcInfoVtablePos :: Maybe Int
    }

data ClassInfo = ClassInfo
    { _classInfoDecl :: AST.ClassDecl
    , _classFields :: Map.Map AST.Ident ClassField
    , _classMethods :: Map.Map AST.Ident FuncInfo
    }

data ClassField = ClassField
    { _classFieldType :: AST.Type
    , _classFieldId :: Int
    }

newtype VariableId = VariableId { getVariableId :: Int }
    deriving (Eq, Show)

type GIRMonad m = (MonadState GIRState m, MonadReader GIREnv m)

data Variable = Variable
    { _varType :: !AST.Type
    , _varId :: {-# UNPACK #-} !VariableId
    , _varState :: !VarState
    , _varBound :: !(AST.Located AST.LocalDecl)
    }
    deriving Show

data VarState
    = VarUsable
    | VarUndefined
    deriving (Eq, Show)

makeLenses ''GIRState
makeLenses ''GIREnv
makeLenses ''FuncInfo
makeLenses ''ClassInfo
makeLenses ''ClassField
makeLenses ''Variable

instance HasMiddleEndState GIRState where
    middleEndState = girME

transLocalDecl :: GIRMonad m => AST.LocalDecl -> m ()
transLocalDecl decl = mapM_ go $ decl ^. AST.localDeclItems
  where
    go item = girVariables . at (item ^. AST.obj . AST.localDeclName) . singular _Just . varState .= VarUsable

finishBlockStartNew :: GIRMonad m => Name -> m ()
finishBlockStartNew newBlockName = do
    block <- use girCurrentBlock
    addBlock block{_blockBody = reverse $ block ^. blockBody}
    girCurrentBlock .= Block
        { _blockName = newBlockName
        , _blockPhi = []
        , _blockBody = []
        , _blockEnd = BlockEndNone
        }

newBasicBlock :: GIRMonad m => Name -> m () -> m ()
newBasicBlock blockName act = do
    oldBlock <- use girCurrentBlock

    girCurrentBlock .= Block
        { _blockName = blockName
        , _blockPhi = []
        , _blockBody = []
        , _blockEnd = BlockEndNone
        }
    act
    finishBlockStartNew blockName

    girCurrentBlock .= oldBlock

assertReachableStmt :: GIRMonad m => AST.Located AST.Stmt -> m ()
assertReachableStmt stmt = use (girCurrentBlock . blockEnd) >>= \case
    BlockEndNone -> pure ()
    _ -> simpleError stmt $ "unreachable statement"

whenReachable :: GIRMonad m => m () -> m ()
whenReachable act = use (girCurrentBlock . blockEnd) >>= \case
    BlockEndNone -> act
    _ -> pure ()

transStmt :: GIRMonad m => AST.Located AST.Stmt -> m ()
transStmt (AST.Located _ (AST.StmtBlock stmts)) = undefined --TODO
transStmt st@(AST.Located l (AST.StmtAssign lval expr)) = do
    (lvalTy, lvalMem) <- transLval lval
    (exprTy, exprOperand) <- transExpr expr
    checkTypeImplicitConv st lvalTy exprTy
    void $ emitInstr Nothing (IStore $ Store lvalMem (sizeOf lvalTy) exprOperand) l [InstrComment $ pPrint st]
transStmt st@(AST.Located l stmt@(AST.StmtReturn Nothing)) =
    view (girCurrentFunction . singular _Just . AST.funcRetType) >>= \case
        AST.TyVoid -> setEnd BlockEndReturnVoid
        _ -> simpleError st "return in a function returning non-void"
transStmt st@(AST.Located l stmt@(AST.StmtReturn (Just expr))) = do
    retTy <- view (girCurrentFunction . singular _Just . AST.funcRetType) 
    (ty, operand) <- transExpr expr
    checkTypeImplicitConv l retTy retTy
    setEnd $ BlockEndReturn operand
transStmt st@(AST.Located l (AST.StmtInc lval)) = do
    (ty, memory) <- transLval lval
    checkTypeEqual st AST.TyInt ty
    void $ emitInstr Nothing (IIncDec $ Inc memory (sizeOf ty)) l [InstrComment $ "increment of " <+> pPrint lval]
transStmt st@(AST.Located l (AST.StmtDec lval)) = do
    (ty, memory) <- transLval lval
    checkTypeEqual st AST.TyInt ty
    void $ emitInstr Nothing (IIncDec $ Dec memory (sizeOf ty)) l [InstrComment $ "decrement of " <+> pPrint lval]
transStmt st@(AST.Located l (AST.StmtIf cond ifTrue mifFalse)) = do
    operand <- transExprTypeEqual AST.TyBool cond

    endName <- mkName $ Just "ifEnd"
    trueBranchName <- mkName $ Just "ifTrue"
    falseBranchName <- case mifFalse of
        Nothing -> pure endName
        Just _ -> mkName $ Just "ifFalse"

    setEnd $ BlockEndBranchCond operand trueBranchName falseBranchName

    finishBlockStartNew trueBranchName
    transStmt ifTrue
    setEnd $ BlockEndBranch endName

    forM_ mifFalse $ \ifFalse -> do
        finishBlockStartNew falseBranchName
        transStmt ifFalse
        setEnd $ BlockEndBranch endName

    finishBlockStartNew endName
transStmt st@(AST.Located l (AST.StmtWhile cond body)) = do
    condName <- mkName $ Just "whileCond"
    bodyName <- mkName $ Just "whileBody"
    endName <- mkName $ Just "whileEnd"

    setEnd $ BlockEndBranch condName
    finishBlockStartNew condName

    (ty, operand) <- transExpr cond
    checkTypeEqual cond AST.TyBool ty
    setEnd $ BlockEndBranchCond operand bodyName endName
    finishBlockStartNew bodyName

    transStmt body
    setEnd $ BlockEndBranch condName
    finishBlockStartNew endName
transStmt st@(AST.Located l (AST.StmtFor ty idx array body)) = undefined -- TODO
transStmt st@(AST.Located l (AST.StmtExpr expr)) = void $ transExpr (AST.Located l expr)
transStmt st@(AST.Located l (AST.StmtDecl decl)) = forM_ (decl ^. AST.localDeclItems) go
  where
    expectedType = decl ^. AST.localDeclType

    go :: GIRMonad m => AST.Located AST.LocalDeclItem -> m ()
    go locItem@(AST.Located li item) = do
        girVariables . at (item ^. AST.localDeclName) . singular _Just . varState .= VarUsable
        var <- use $ girVariables . at (item ^. AST.localDeclName) . singular _Just
        (ty, operand) <- case item ^. AST.localDeclValue of
            Just expr -> transExpr expr
            Nothing -> case expectedType of
                AST.TyString -> do
                    emptyStr <- emitInstr (Just "emptyString") (IGetAddr (GetAddr $ MemoryGlobal "LATC_emptyString")) li []
                    pure (AST.TyString, OperandNamed emptyStr)
                _ -> pure (expectedType, OperandInt 0)
        checkTypeImplicitConv locItem expectedType ty
        void $ emitInstr Nothing
            (IStore (Store (MemoryLocal . getVariableId $ var ^. varId) (sizeOf ty) operand))
            li [InstrComment $ "initialize" <+> pPrint item]
transStmt st@(AST.Located l AST.StmtNone) = pure ()

transExpr :: GIRMonad m => AST.Located AST.Expr -> m (AST.Type, Operand)
transExpr   (AST.Located l (AST.ExprLval lval)) = do
    (ty, memory) <- transLval (AST.Located l lval)
    value <- emitInstr Nothing (ILoad (Load memory (sizeOf ty))) l [InstrComment $ "load" <+> pPrint lval]
    pure (ty, OperandNamed value)
transExpr    (AST.Located _ (AST.ExprInt i)) = pure (AST.TyInt, OperandInt i)
transExpr    (AST.Located _ (AST.ExprString str)) = undefined -- TODO
transExpr    (AST.Located _ AST.ExprTrue) = pure (AST.TyBool, OperandInt 1)
transExpr    (AST.Located _ AST.ExprFalse) = pure (AST.TyBool, OperandInt 0)
transExpr    (AST.Located _ AST.ExprNull) = pure (AST.TyNull, OperandInt 0)
transExpr ex@(AST.Located l (AST.ExprCall funLval argExprs)) =
    transFunCall funLval >>= \case
        Nothing -> do
            simpleError ex $ "Unresolved function, unable to call"
            pure (AST.TyInt, OperandInt 0) -- TODO
        Just (dest, info) -> do
            checkCallArgumentsLength ex (info ^. funcInfoDecl ^. AST.funcArgs) argExprs
            args <- zipWithM prepareArg (info ^. funcInfoDecl ^. AST.funcArgs) argExprs
            ret <- emitInstr Nothing (ICall $ Call dest args) l [InstrComment $ "call" <+> pPrint ex]
            pure (info ^. funcInfoDecl ^. AST.funcRetType, OperandNamed ret)
  where
     prepareArg :: GIRMonad m => AST.FunArg -> AST.Located AST.Expr -> m Operand
     prepareArg arg expr = do
        (ty, operand) <- transExpr expr
        checkTypeImplicitConv expr (arg ^. AST.funArgType) ty 
        pure operand
transExpr ex@(AST.Located l (AST.ExprUnOp AST.UnOpNot arg)) = do
    operand <- transExprTypeEqual AST.TyBool arg
    ret <- emitInstr (Just "not") (IUnOp (UnOp UnOpNot operand)) l []
    pure (AST.TyBool, OperandNamed ret)
transExpr ex@(AST.Located l (AST.ExprUnOp AST.UnOpNeg arg)) = do
    operand <- transExprTypeEqual AST.TyInt arg
    ret <- emitInstr (Just "neg") (IUnOp (UnOp UnOpNeg operand)) l []
    pure (AST.TyInt, OperandNamed ret)
transExpr (AST.Located _ (AST.ExprBinOp lhs AST.BinOpAnd rhs)) = do
    endName <- mkName $ Just "andEnd"
    midName <- mkName $ Just "andMid"
    name <- use $ girCurrentBlock . blockName

    operandLhs <- transExprTypeEqual AST.TyBool lhs
    setEnd $ BlockEndBranchCond operandLhs midName endName
    finishBlockStartNew midName

    operandRhs <- transExprTypeEqual AST.TyBool rhs
    setEnd $ BlockEndBranch endName
    finishBlockStartNew endName

    value <- emitPhi (Just "and") [PhiBranch name operandLhs, PhiBranch midName operandRhs]
    pure (AST.TyBool, OperandNamed value)
transExpr (AST.Located _ (AST.ExprBinOp lhs AST.BinOpOr rhs)) = do
    endName <- mkName $ Just "orEnd"
    midName <- mkName $ Just "orMid"
    name <- use $ girCurrentBlock . blockName

    operandLhs <- transExprTypeEqual AST.TyBool lhs
    setEnd $ BlockEndBranchCond operandLhs endName midName
    finishBlockStartNew midName

    operandRhs <- transExprTypeEqual AST.TyBool rhs
    setEnd $ BlockEndBranch endName
    finishBlockStartNew endName

    value <- emitPhi (Just "or") [PhiBranch name operandLhs, PhiBranch midName operandRhs]
    pure (AST.TyBool, OperandNamed value)
transExpr ex@(AST.Located l (AST.ExprBinOp lhs op rhs)) = do
    (tyLhs, operandLhs) <- transExpr lhs
    (tyRhs, operandRhs) <- transExpr rhs

    let emit op = OperandNamed <$> 
            emitInstr Nothing (IBinOp (BinOp operandLhs op operandRhs)) l [InstrComment $ pPrint ex]

    case (tyLhs, op, tyRhs) of
        (_, AST.BinOpEqual, _) -> do
            checkTypesComparable ex tyLhs tyRhs
            (AST.TyBool,) <$> emit BinOpEqual
        (_, AST.BinOpNotEqual, _) -> do
            checkTypesComparable ex tyLhs tyRhs
            (AST.TyBool,) <$> emit BinOpNotEqual
        (AST.TyInt, intOp -> Just (retTy, op), AST.TyInt) ->
            (retTy,) <$> emit op
        _ -> do
            simpleError ex "Invalid binary operator application"
            pure (AST.TyInt, OperandInt 0)
  where
    intOp AST.BinOpPlus = Just (AST.TyInt, BinOpPlus)
    intOp AST.BinOpMinus = Just (AST.TyInt, BinOpMinus)
    intOp AST.BinOpTimes = Just (AST.TyInt, BinOpTimes)
    intOp AST.BinOpDivide = Just (AST.TyInt, BinOpDivide)
    intOp AST.BinOpModulo = Just (AST.TyInt, BinOpModulo)
    intOp AST.BinOpLess = Just (AST.TyBool, BinOpLess)
    intOp AST.BinOpLessEqual = Just (AST.TyBool, BinOpLessEqual)
    intOp AST.BinOpGreater = Just (AST.TyBool, BinOpGreater)
    intOp AST.BinOpGreaterEqual = Just (AST.TyBool, BinOpGreaterEqual)
    intOp _ = Nothing
transExpr ex@(AST.Located _ (AST.ExprNew ty)) = undefined
transExpr ex@(AST.Located _ (AST.ExprNewArr ty len)) = undefined
transExpr ex@(AST.Located _ (AST.ExprCast targetTy expr)) = do
    (ty, operand) <- transExpr expr
    checkTypeImplicitConv ex targetTy ty
    pure (targetTy, operand)

transExprTypeEqual :: GIRMonad m => AST.Type -> AST.Located AST.Expr -> m Operand
transExprTypeEqual expectedType expr = do
    (ty, operand) <- transExpr expr
    checkTypeEqual expr expectedType ty
    pure operand

transLval :: GIRMonad m => AST.Located AST.Lval -> m (AST.Type, Memory)
transLval lval@(AST.Located _ (AST.LvalVar ident)) =
    use (girVariables . at ident) >>= \case
        Nothing -> do
            simpleError lval $ "Unbound variable" <+> pPrint ident
            pure (AST.TyInt, MemoryLocal 0)
        Just var -> do
            case var ^. varState of
                VarUndefined -> simpleError lval $ "Variable referenced before definition"
                VarUsable -> pure ()
            pure (var ^. varType, MemoryLocal . getVariableId $ var ^. varId)
transLval lval@(AST.Located _ (AST.LvalArray arrExpr idxExpr)) = do
    (tyArr, operandArr) <- transExpr arrExpr
    operandIdx <- transExprTypeEqual AST.TyInt idxExpr
    case tyArr of
        AST.TyArray ty ->
            pure (ty, MemoryOffset (MemoryPointer operandArr) operandIdx (sizeOf ty))
        ty -> do
            simpleError lval $ "Not an array: " <+> pPrint ty
            pure (AST.TyInt, MemoryLocal 0)
transLval lval@(AST.Located _ (AST.LvalField objExpr field)) = do
    (tyObj, operandObj) <- transExpr objExpr
    case tyObj of
        AST.TyClass className ->
            view (girClasses . at className . singular _Just . classFields . at field) >>= \case
                Nothing -> do
                    simpleError lval $ pPrint className <+> "has no field" <+> pPrint field
                    pure (AST.TyInt, MemoryLocal 0)
                Just field ->
                    pure (field ^. classFieldType, MemoryField (MemoryPointer operandObj) (field ^. classFieldId))
        AST.TyString -> do
            unless (field == "length") . simpleError lval $ "string has no field" <+> pPrint field
            pure (AST.TyInt, MemoryField (MemoryPointer operandObj) 0)
        AST.TyArray _ -> do
            unless (field == "length") . simpleError lval $ pPrint tyObj <+> "has no field" <+> pPrint field
            pure (AST.TyInt, MemoryField (MemoryPointer operandObj) 0)
        _ -> do
            simpleError lval $ pPrint tyObj <+> "has no fields"
            pure (AST.TyInt, MemoryField (MemoryPointer operandObj) 0)

transFunCall :: GIRMonad m => AST.Located AST.Lval -> m (Maybe (CallDest, FuncInfo))
transFunCall (AST.Located _ (AST.LvalVar name)) = view (girFunctions . at name) >>= \case
    Nothing -> pure Nothing
    Just info -> pure . Just $ case info ^. funcInfoVtablePos of
        Nothing -> (CallDestFunction (Ident $ AST.getIdent name), info)
        Just offset -> (CallDestVirtual MemoryThis offset, info)
transFunCall lval@(AST.Located _ (AST.LvalField objExpr name)) = do
    (objTy, objOperand) <- transExpr objExpr
    case objTy of
        AST.TyClass className ->
            view (girClasses . at className . singular _Just . classMethods . at name) >>= \case
                Nothing -> do
                    simpleError lval $ pPrint className <+> "has no method" <+> pPrint name
                    pure Nothing
                Just info ->
                    pure $ Just (CallDestVirtual (MemoryPointer objOperand) (info ^. funcInfoVtablePos . singular _Just), info)
        _ -> do
            simpleError lval $ "not a class"
            pure Nothing
transFunCall lval@(AST.Located _ (AST.LvalArray _ _)) = do
    simpleError lval "Not a function"
    pure Nothing

simpleError :: (GIRMonad m, Reportible r) => r -> Doc -> m ()
simpleError ctx head = report Diagnostic
    { _diagWhere = Just $ ctx ^. AST.locRange
    , _diagContent = hang head 4 (pPrint ctx)
    , _diagType = DiagnosticError
    , _diagNotes = []
    }

checkCallArgumentsLength :: (GIRMonad m, Reportible r) => r -> [a] -> [b] -> m ()
checkCallArgumentsLength ctx expected got = unless (length expected == length got) . simpleError ctx $ hsep
    [ "Argument count mismatch: expected"
    , int (length expected)
    , "but got"
    , int (length got)
    ]

checkTypeEqual :: (GIRMonad m, Reportible r) => r -> AST.Type -> AST.Type -> m ()
checkTypeEqual ctx expected got = unless (expected == got) . simpleError ctx $ hsep
    [ "Type mismatch: expected"
    , pPrint expected
    , "but got:"
    , pPrint got
    ]

checkTypeImplicitConv :: (GIRMonad m, Reportible r) => r -> AST.Type -> AST.Type -> m ()
checkTypeImplicitConv ctx expected got = unless (convertible expected got) . simpleError ctx $ hsep
    [ "Type mismatch: expected"
    , pPrint expected
    , "but got:"
    , pPrint got
    ]
  where
    -- TODO inheritance
    convertible (AST.TyClass _) AST.TyNull = True
    convertible t t' = t == t'
    
checkTypesComparable :: (GIRMonad m, Reportible r) => r -> AST.Type -> AST.Type -> m ()
checkTypesComparable ctx expected got = unless (comparable expected got) . simpleError ctx $ hsep
    [ "Type mismatch: cannot compare"
    , pPrint expected
    , "with"
    , pPrint got
    ]
  where
    comparable AST.TyNull (AST.TyClass _) = True
    comparable (AST.TyClass _) AST.TyNull = True
    comparable t t' = t == t'

emitInstr :: GIRMonad m => Maybe Ident -> InstrPayload -> AST.LocRange -> [InstrMetadata] -> m Name
emitInstr humanName payload loc meta = do
    result <- mkName humanName
    girCurrentBlock . blockBody %= cons (Instruction result payload (Just loc) meta)
    pure result

emitPhi :: GIRMonad m => Maybe Ident -> [PhiBranch] -> m Name
emitPhi humanName branches = do
    result <- mkName humanName
    girCurrentBlock . blockPhi %= cons (PhiNode result branches)
    pure result

setEnd :: GIRMonad m => BlockEnd -> m ()
setEnd end = whenReachable $ girCurrentBlock . blockEnd .= end

sizeOf :: AST.Type -> Size
sizeOf AST.TyInt = Size32
sizeOf AST.TyBool = Size8
sizeOf AST.TyVoid = Size0
sizeOf AST.TyString = SizePtr
sizeOf (AST.TyArray _) = SizePtr
sizeOf (AST.TyClass _) = SizePtr
sizeOf AST.TyNull = SizePtr
