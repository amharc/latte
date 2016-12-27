{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Latte.Middleend.GenIR where

import Control.Lens
import Control.Monad
import qualified Data.Map as Map
import qualified Language.Latte.Frontend.AST as AST
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

data GIRState = GIRState
    { _girME :: !MiddleEndState
    , _girVariableCnt :: !Int
    , _girVariables :: Map.Map Ident Variable
    , _girCurrentBlock :: Block
    }

data GIREnv = GIREnv
    { _girCurrentFunction :: Maybe AST.FuncDecl
    , _girCurrentClass :: Maybe AST.ClassDecl
    , _girFunctions :: Map.Map Ident FuncInfo
    , _girClasses :: Map.Map Ident ClassInfo
    }

data FuncInfo = FuncInfo
    { _funcInfoDecl :: AST.FuncDecl
    , _funcInfoVtablePos :: Maybe Int
    }

data ClassInfo = ClassInfo
    { _classInfoDecl :: AST.ClassDecl
    , _classFields :: Map.Map Ident ClassField
    , _classMethods :: Map.Map Ident FuncInfo
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

transLocalDecl :: GIRMonad m => LocalDecl -> m ()
transLocalDecl decl = mapM go $ decl ^. localDeclItems
  where
    go item = girVariables . at (item ^. obj . localDeclName) . singular _Just . varState %= VarUsable


finishBlock :: GIRMonad m => Name -> m ()
finishBlock newBlockName = do
    block <- view girCurrentBlock
    zoom girME $ addBlock block{_blockBody = reverse $ block ^. blockBody}
    girCurrentBlock .= Block
        { _blockName = newBlockName
        , _blockPhi = []
        , _blockInstr = []
        , _blockEnd = BlockEndNone
        }

newBasicBlock :: GIRMonad m => Name -> m () -> m ()
newBasicBlock blockName act = do
    oldBlock <- view girCurrentBlock

    girCurrentBlock .= Block
        { _blockName = newBlockName
        , _blockPhi = []
        , _blockInstr = []
        , _blockEnd = BlockEndNone
        }
    act
    finishBlock oldBlockName

    girCurrentBlock .= oldBlock
    pure blockName

assertReachableStmt :: GIRMonad m => AST.Located AST.Stmt -> m ()
assertReachableStmt stmt = view girCurrentBlockEnd >>= \case
    BlockEndNone -> pure ()
    _ -> report Diagnostic
        { _diagWhere = Just (stmt ^. loc)
        , _diagContent = hang "unreachable statement" 4 (pPrint $ stmt ^. obj)
        , _diagType = DiagnosticError
        , _diagNotes = []
        }

whenReachable :: GIRMonad m => m a -> m a
whenReachable act = view gitCurrentBlockEnd >>= \case
    BlockEndNone -> act
    _ -> pure ()

transStmt :: GIRMonad m => Located Stmt -> m ()
transStmt (AST.Loc (AST.StmtBlock stmts)) = undefined --TODO
transStmt st@(AST.Located l stmt@(AST.StmtReturn Nothing)) =
    view (gitCurrentFunction . AST.funcRetType) >>= \case
        AST.TyVoid -> setEnd BlockEndReturnVoid
        _ -> report Diagnostic
            { _diagWhere = l
            , _diagContent = hang "return in a function returning non-void" 4 (pPrint $ stmt)
            , _diagType = DiagnosticError
            , _diagNotes = []
            }
transStmt st@(AST.Located l stmt@(AST.StmtReturn (Just expr))) = do
    retTy <- view (gitCurrentFunction . AST.funcRetType) 
    (ty, operand) <- transExpr expr
    checkTypeImplicitConv l retTy retTy
    setEnd $ BlockEndReturn operand
transStmt st@(AST.Located l (AST.StmtInc lval)) = do
    (ty, memory) <- transLval lval
    checkTypeEqual st AST.TyInt ty
    void $ emitInstr Nothing (IIncDec $ Inc memory (sizeOf ty)) l [InstrComment $ "increment of " <+> lval ^. obj]
transStmt st@(AST.Located l (AST.StmtDec lval)) = do
    (ty, memory) <- transLval lval
    checkTypeEqual st AST.TyInt ty
    void $ emitInstr Nothing (IIncDec $ Dec memory (sizeOf ty)) l [InstrComment $ "decrement of " <+> lval ^. obj]
transStmt st@(AST.Located l (AST.StmtIf cond ifTrue mifFalse)) = do
    operand <- transExprTypeEqual AST.TyBool cond

    endName <- mkName $ Just "ifEnd"
    trueBranchName <- mkName $ Just "ifTrue"
    falseBranchName <- case mifFalse of
        Nothing -> pure endName
        Just _ -> mkName $ Just "ifFalse"

    setEnd $ BlockEndBranchCond operand trueBranchName falseBranchName

    finishBlock trueBranchName
    transStmt ifTrue
    setEnd $ BlockEndBranch endName

    forM mifFalse $ \ifFalse ->
        finishBlock falseBranchName
        transStmt ifFalse
        setEnd $ BlockEndBranch endName

    finishBlock endName
transStmt st@(AST.Located l (AST.StmtWhile cond body)) = do
    condName <- mkName $ Just "whileCond"
    bodyName <- mkName $ Just "whileBody"
    endName <- mkName $ Just "whileEnd"

    setEnd BlockEndBranch condName
    finishBlock condName

    (ty, operand) <- transExpr cond
    checkTypeEqual cond AST.TyBool ty
    setEnd $ BlockEndBranchCond operand bodyName endName
    finishBlock bodyName

    transStmt body
    setEnd $ BlockEndBranch condName
    finishBlock endName
transStmt st@(AST.Located l (AST.StmtFor ty idx array body)) = undefined -- TODO
transStmt st@(AST.Located l (AST.StmtExpr expr)) = void $ transExpr (AST.Located l expr)
transStmt st@(AST.Located l (AST.StmtDecl decl)) = forM (decl ^. localDeclItems) go
  where
    expectedType = decl ^. localDeclType

    go locItem@(AST.Located li item) = do
        girVariable . at (item ^. localDeclName) . singular _Just . varState %= VarUsable
        var <- view $ girVariable . at (item ^. localDeclName) . singular _Just
        (ty, operand) <- case item ^. localDeclValue of
            Just expr -> transExpr expr
            Nothing -> case expectedType of
                TyString -> do
                    emptyStr <- emitInstr (Just "emptyString") (IGetAddr (MemoryGlobal "LATC_emptyString")) li []
                    pure (TyString, OperandNamed emptyStr)
                _ -> pure (expectedTy, OperandInt 0)
        checkTypeImplicitConv locItem expectedType ty
        emitInstr (Just "initialize") (IStore (Store (MemoryLocal $ var ^. variableId) (sizeOf ty) operand))
transStmt st@(AST.Located l AST.StmtNone) = pure ()

transExpr :: GIRMonad m => AST.Located AST.Expr -> m (AST.Type, Operand)
transExpr   (AST.Located l (AST.ExprLval lval)) = do
    (ty, memory) <- transLval (AST.Located l lval)
    value <- emitInstr Nothing (ILoad (Load memory (sizeOf ty)))
    pure (ty, OperandNamed value)
transExpr    (AST.Loc (AST.ExprInt i)) = pure (TyInt, OperandInt i)
transExpr    (AST.Loc (AST.ExprString str)) = undefined -- TODO
transExpr    (AST.Loc AST.ExprTrue) = pure (TyBool, OperandInt 1)
transExpr    (AST.Loc AST.ExprFalse) = pure (TyBool, OperandInt 0)
transExpr    (AST.Loc AST.ExprNull) = pure (TyNull, OperandInt 0)
transExpr ex@(AST.Located l (AST.ExprCall funLval argExprs)) =
    transFunCall funLval >>= \case
    use (girFunctions . at funName) >>= \case
        Nothing -> do
            unboundFunction ex funName
            pure (AST.TyInt, OperandInt 0) -- TODO
        Just (dest, info) -> do
            checkCallArgumentsLength ex (info ^. funcInfoDecl ^. funcDeclArgs) argExprs
            args <- zipWithM prepareArg (info ^. funcInfoDecl ^. funcDeclArgs) argExprs
            dest <- transCallDest 
            emitInstr Nothing (ICall $ Call dest args) l [InstrComment $ "call" <+> pPrint ex]
  where
     prepareArg :: AST.FunArg -> Located AST.Expr -> m Operand
     prepareArg arg expr = do
        (ty, operand) <- transExpr expr
        checkTypeImplicitConv expr ty 
        pure operand
transExpr ex@(AST.Loc (AST.ExprUnOp AST.UnOpNot arg)) =
    operand <- transExprTypeEqual ex AST.TyBool arg
    ret <- emitInstr (Just "not") (IUnOp (UnOp UnOpNot operand))
    pure (ty, OperandNamed ret)
transExpr ex@(AST.Loc (AST.ExprUnOp AST.UnOpNeg arg)) =
    operand <- transExprTypeEqual ex AST.TyInt arg
    ret <- emitInstr (Just "neg") (IUnOp (UnOp UnOpNeg operand))
    pure (ty, OperandNamed ret)
transExpr (AST.Loc (AST.ExprBinOp lhs AST.BinOpAnd rhs)) =
    endName <- mkName $ Just "andEnd"
    midName <- mkName $ Just "andMid"
    name <- view $ girCurrentBlock ^. blockName

    operandLhs <- transExprTypeEqual AST.TyBool lhs
    setEnd $ BlockEndBranchCond operandLhs midName endName
    finishBlock midName

    operandRhs <- transExprTypeEqual AST.TyBool rhs
    setEnd $ BlockEndBranch endName
    finishBlock endName

    value <- emitPhi (Just "and") [PhiBranch name operandLhs, PhiBranch midName operandRhs]
    pure (AST.TyBool, OperandNamed value)
transExpr (AST.Loc (AST.ExprBinOp lhs AST.BinOpAnd rhs)) =
    endName <- mkName $ Just "orEnd"
    midName <- mkName $ Just "orMid"
    name <- view $ girCurrentBlock ^. blockName

    operandLhs <- transExprTypeEqual AST.TyBool lhs
    setEnd $ BlockEndBranchCond operandLhs endName midName
    finishBlock midName

    operandRhs <- transExprTypeEqual AST.TyBool rhs
    setEnd $ BlockEndBranch endName
    finishBlock endName

    value <- emitPhi (Just "or") [PhiBranch name operandLhs, PhiBranch midName operandRhs]
    pure (AST.TyBool, OperandNamed value)
transExpr ex@(AST.Location l (AST.ExprBinOp lhs op rhs)) =
    (tyLhs, operandLhs) <- transExpr lhs
    (tyRhs, operandRhs) <- transExpr rhs
    case (tyLhs, op, tyRhs) of
        (_, AST.BinOpEqual, _) -> do
            checkTypesComparable tyLhs tyRhs
            (AST.TyBool,) <$> emit BinOpEqual
        (_, AST.BinOpNotEqual, _) -> do
            checkTypesComparable tyLhs tyRhs
            (AST.TyBool,) <$> emit BinOpNotEqual
        (AST.TyInt, intOp -> Just (retTy, op), AST.TyInt) ->
            (retTy,) <$> emit op
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

    emit op = OperandNamed <$> emitInstr Nothing (IBinOp (BinOp operandLhs BinOpEqual operandRhs)) l [InstrComment $ pPrint ex]
transExpr ex@(AST.Loc (AST.ExprNew ty)) = undefined
transExpr ex@(AST.Loc (AST.ExprNewArr ty)) = undefined
transExpr ex@(AST.Loc (AST.ExprCast targetTy expr)) = do
    (ty, operand) <- transExpr expr
    checkTypeImplicitConv ex targetTy ty
    pure (targetTy, operand)

transExprTypeEqual :: GIRMonad m => AST.Type -> AST.Located AST.Expr -> m Operand
transExprTypeEqual expectedType expr = do
    (ty, operand) <- transExpr expr
    checkTypeEqual expr expectedType ty
    pure operand

transLval :: AST.Located AST.Lval -> m (AST.Type, Memory)
transLval lval@(AST.Loc (AST.LvalVar ident)) =
    view (girVariables . at ident) >>= \case
        Nothing -> simpleError lval $ "Unbound variable" <+> pPrint ident
        Just var -> do
            case var ^. varState of
                VarUndefined -> unboundVariable lval ident
                VarUsable -> pure ()
            pure (var ^. varType, MemoryLocal $ var ^. varId)
transLval lval@(AST.Loc (AST.LvalArray arrExpr idxExpr)) =
    (tyArr, operandArr) <- transExpr arrExpr
    operandIdx <- transExprTypeEqual AST.TyInt idxExpr
    case tyArr of
        TyArray ty ->
            pure (ty, MemoryOffset (MemoryPointer operandArr) operandIdx (sizeOf ty))
        ty -> do
            simpleError lval $ "Not an array: " <+> pPrint ty
            pure (TyInt, MemoryLocal 0)
transLval lval@(AST.Loc (AST.LvalField objExpr field)) =
    (tyObj, operandObj) <- transExpr objExpr
    case tyObj of
        TyClass className ->
            use (girClasses . at className . singular _Just . classFields . at name) >>= \case
                Nothing -> do
                    simpleError lval $ pPrint className <+> "has no field" <+> pPrint name
                    pure (TyInt, MemoryLocal 0)
                Just field ->
                    pure (field ^. classFieldType, MemoryField (MemoryPointer operandObj) (field ^. classFieldId))
        TyString -> do
            unless (field == "length") . simpleError lval $ "string has no field" <+> pPrint name
            pure (AST.TyInt, MemoryField (MemoryPointer operandObj) 0)
        TyArray _ -> do
            unless (field == "length") . simpleError lval $ pPint tyObj <+> "has no field" <+> pPrint name
            pure (AST.TyInt, MemoryField (MemoryPointer operandObj) 0)
        _ -> do
            simpleError lval $ pPrint tyObj <+> "has no fields"
            pure (AST.TyInt, MemoryField (MemoryPointer operandObj) 0)

transFunCall :: AST.Located AST.Lval -> m (Maybe (CallDest, FuncInfo))
transFunCall (AST.Located _ (AST.LvalVar name)) = use (girFunctions . at name) >>= \case
    Nothing -> pure Nothing
    Just info -> pure . Just $ case info ^. funcInfoVtablePos of
        Nothing -> (CallDestFunction name, info)
        Just offset -> (CallDestVirtual MemoryThis, info)
transFunCall lval@(AST.Located _ (AST.LvalField objExpr name)) = do
    (objTy, objOperand) <- transExpr objExpr
    case objTy of
        TyClass className ->
            use (girClasses . at className . singular _Just . classMethods . at name) >>= \case
                Nothing -> simpleError lval $ pPrint className <+> "has no method" <+> pPrint name
                Just info -> (CallDestVirtual (MemoryPointer objOperand) (info ^. funcInfoVtablePos . singular _Just), info)
transFunCall lval@(AST.Located _ (AST.LvalArray _ _)) =
    simpleError lval $ pPrint "Not a function"

simpleError :: Reportible r => r -> Doc -> m ()
simpleError ctx head = report Diagnostic
    { _diagWhere = ctx ^. loc
    , _diagContent = hang head 4 (pPrint ctx)
    , _diagType = DiagnosticError
    , _diagNotes = []
    }

unboundVariable :: Reportible r => r -> Ident -> m ()
unboundVariable ctx varName =      
unboundFunction :: Reportible r => r -> Ident -> m ()
unboundFunction ctx funName = simpleError ctx $ "Unbound function" <+> pPrint funName

checkCallArgumentsLength :: Reportible r => r -> [a] -> [b] -> m ()
checkCallArgumentsLength ctx expected got
    | length expected == length got = pure ()
    | otherwise = simpleError ctx $ hsep
        [ "Argument count mismatch: expected"
        , int (length expected)
        , "but got",
        , int (length got)
        ]

checkTypeEqual :: Reportible r => r -> AST.Type -> AST.Type -> m ()
checkTypeEqual ctx expected got
    | expected == got = pure ()
    | otherwise = simpleError ctx $ hsep
        [ "Type mismatch: expected"
        , pPrint expected
        , "but got:",
        , pPrint got
        ]

checkTypeImplicitConv :: Reportible r => r -> AST.Type -> AST.Type -> m ()
checkTypeImplicitConv ctx expected got
    | convertible expected got = pure ()
    | otherwise = simpleError ctx $ hsep
        [ "Type mismatch: expected"
        , pPrint expected
        , "but got:",
        , pPrint got
        ]
  where
    -- TODO inheritance
    convertible (TyClass _) TyNull = True
    convertible t t' = t == t'
    
checkTypeComparable :: Reportible r => r -> AST.Type -> AST.Type -> m ()
checkTypeComparable ctx expected got
    | comparale expected got = pure ()
    | otherwise = simpleError ctx $ hsep
        [ "Type mismatch: cannot compare"
        , pPrint expected
        , "with",
        , pPrint got
        ]
  where
    comparable TyNull (TyClass _) = True
    comparable (TyClass _) TyNull = True
    comparable t t' = t == t'

emitInstr :: GIRMonad m => Maybe Ident -> InstrPayload -> Frontend.LocRange -> [InstrMetadata] -> m Name
emitInstr humanName payload loc meta = do
    result <- mkName humanName
    girCurrentBlock . blockInstr %= cons (Instruction result payload loc meta)
    pure result

emitPhi :: GIRMonad m => Maybe Ident -> [PhiBranch] -> m Name
emitPhi humanName branches = do
    result <- mkName humanName
    girCurrentBlock . blockPhi %= cons (PhiNode result branches)
    pure result

setEnd :: GIRMonad m => BlockEnd -> m ()
setEnd end = whenReachable $ girCurrentBlockEnd .= end

sizeOf :: AST.Type -> Size
sizeOf AST.TyInt = Size32
sizeOf AST.TyBool = Size8
sizeof AST.TyVoid = Size0
sizeof AST.TyString = SizePtr
sizeof AST.TyArray = SizePtr
sizeof (AST.TyClass _) = SizePtr

{-
  where
    go item =
        view (girVariables . at (item ^. obj . localDeclName)) >>= \case
            Just (Variable old) | old ^. varstate == VarUndefined ->
                report Diagnostic
                    { _diagWhere = Just (item ^. loc)
                    , _diagContent = hang
                        ("Variable" <> item ^. obj . localDeclName <> "has already been bound in this scope")
                        4 (pPrint $ item ^. obj)
                    , _diagType = DiagnosticError
                    , _diagNotes = [ Diagnostic
                        { _diagWhere = Just (old ^. varBound . loc)
                        , _diagWhat = hang
                            ("Previous definition was here")
                            4 (Print $ old ^. varBound . obj)
                        , _diagType = DiagnosticNote
                        , _diagNotes = []
                        }]
                    }
            Just (Variable old) ->
                report Diagnostic
                    { _diagWhere = Just (item ^. loc)
                    , _diagContent = hang
                        ("Variable" <> item ^. obj . localDeclName <> "shadows a variable from the outer scope")
                        4 (pPrint $ item ^. obj)
                    , _diagType = DiagnosticWarn
                    , _diagNotes = [ Diagnostic
                        { _diagWhere = Just (old ^. varBound . loc)
                        , _diagWhat = hang
                            ("Previous definition was here")
                            4 (Print $ old ^. varBound . obj)
                        , _diagType = DiagnosticNote
                        , _diagNotes = []
                        }]
                    }
            Nothing -> do
                varId <- girVariableCnt <+= 1
                girVariables . at (item ^. obj . localDeclName) ?= Variable
                    { _varType = decl ^. localDeclType
                    , _varId = varId
                    , _varState = VarUsable
                    ,
-}

bindVar :: GITMonad m => AST.Ident -> AST.Type -> AST.Location -> m Variable
bindVar varName ty location = do
    varId <- girVariableCnt <+= 1
    view (girBindings . at name) >>= \case
        Just (Variable old)
            | old ^. varState == VarUndefined -> do
                report Diagnostic
                    { _diagWhere = Just location
                    , _diagWhat
        Nothing -> 
    girBindings . at name ?= Variable ty varId Var

translateLval :: MEMonad m => AST.Lval -> m Memory

