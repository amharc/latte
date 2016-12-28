{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
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
import Data.Coerce
import Data.IORef
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import GHC.Stack
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
    { _funcInfoDecl :: AST.Located AST.FuncDecl
    , _funcInfoVtablePos :: Maybe Int
    }

data ClassInfo = ClassInfo
    { _classInfoDecl :: AST.ClassDecl
    , _classFields :: Map.Map AST.Ident ClassField
    , _classMethods :: Map.Map AST.Ident FuncInfo
    , _classSize :: {-# UNPACK #-} !Int
    }

data ClassField = ClassField
    { _classFieldType :: AST.Type
    , _classFieldId :: Int
    }

type GIRMonad m = (MonadState GIRState m, MonadReader GIREnv m, MonadIO m)

data Variable = Variable
    { _varType :: !AST.Type
    , _varLoc :: VariableLoc
    , _varState :: !VarState
    , _varDefinedIn :: Maybe Block
    }

data VariableLoc = VariableLocal !Int | VariableArgument !Int

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

newBlock :: GIRMonad m => Ident -> m Block
newBlock name = do
   phi <- liftIO $ newIORef []
   body <- liftIO $ newIORef Seq.empty
   end <- liftIO $ newIORef BlockEndNone
   name <- mkName $ Just name
   pure $ Block name phi body end

inBlock :: GIRMonad m => Block -> m a -> m a
inBlock block act = do
    oldBlock <- use girCurrentBlock
    girCurrentBlock .= block
    ret <- act
    girCurrentBlock .= oldBlock
    pure ret

switchToBlock :: GIRMonad m => Block -> m ()
switchToBlock block = girCurrentBlock .= block

assertReachableStmt :: GIRMonad m => AST.Located AST.Stmt -> m ()
assertReachableStmt stmt = isReachable >>= flip unless (simpleError stmt $ "unreachable statement")

whenReachable :: GIRMonad m => m () -> m ()
whenReachable act = isReachable >>= flip when act

isReachable :: GIRMonad m => m Bool
isReachable = use (girCurrentBlock . blockEnd) >>= liftIO . readIORef >>= \case
    BlockEndNone -> pure True
    _ -> pure False

transStmt :: (HasCallStack, GIRMonad m) => AST.Located AST.Stmt -> m ()
transStmt (AST.Located _ (AST.StmtBlock stmts)) = do
    oldVariables <- use girVariables

    forM_ stmts $ \case
        AST.Located l (AST.StmtDecl decl) -> addDecl (AST.Located l decl)
        _ -> pure ()

    forM_ stmts transStmt

    girVariables .= oldVariables
  where
    addDecl decl = forM_ (decl ^. AST.obj ^. AST.localDeclItems) $ \item -> do
        ident <- girVariableCnt <<+= 1
        block <- use girCurrentBlock

        use (girVariables . at (item ^. AST.obj ^. AST.localDeclName)) >>= \case
            Just var | var ^. varDefinedIn == Just block ->
                simpleError item $ "Duplicate definition of variable"
            _ -> pure ()

        girVariables . at (item ^. AST.obj ^. AST.localDeclName) ?= Variable
            { _varType = decl ^. AST.obj ^. AST.localDeclType
            , _varLoc = VariableLocal ident
            , _varState = VarUndefined
            , _varDefinedIn = Just block
            }
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
    checkTypeImplicitConv l retTy ty
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

    endBlock <- newBlock "ifEnd"
    trueBlock <- newBlock "ifTrue"
    falseBlock <- case mifFalse of
        Nothing -> pure endBlock
        Just _ -> newBlock "ifFalse"

    setEnd $ BlockEndBranchCond operand trueBlock falseBlock

    inBlock trueBlock $ do
        transStmt ifTrue
        setEnd $ BlockEndBranch endBlock

    forM_ mifFalse $ \ifFalse -> do
        inBlock falseBlock $ do
            transStmt ifFalse
            setEnd $ BlockEndBranch endBlock

    switchToBlock endBlock
transStmt st@(AST.Located l (AST.StmtWhile cond body)) = do
    condBlock <- newBlock "whileCond"
    bodyBlock <- newBlock "whileBody"
    endBlock <- newBlock "whileEnd"

    setEnd $ BlockEndBranch condBlock

    inBlock condBlock $ do
        (ty, operand) <- transExpr cond
        checkTypeEqual cond AST.TyBool ty
        setEnd $ BlockEndBranchCond operand bodyBlock endBlock

    inBlock bodyBlock $ do
        transStmt body
        setEnd $ BlockEndBranch condBlock

    switchToBlock endBlock
transStmt st@(AST.Located l (AST.StmtFor ty idx array body)) = transStmt $
    AST.Located l $ AST.StmtBlock [declareIndex, declareArray, while]
  where
    declareIndex = arrLoc . AST.StmtDecl $ AST.LocalDecl AST.TyInt
            [arrLoc $ AST.LocalDeclItem "$idx" Nothing]

    declareArray = arrLoc . AST.StmtDecl $ AST.LocalDecl (AST.TyArray ty)
            [arrLoc $ AST.LocalDeclItem "$arr" (Just array)]

    while = stLoc $ AST.StmtWhile (stLoc whileCond) (stLoc whileBody)

    whileCond = AST.ExprBinOp
        (arrLoc . AST.ExprLval $ AST.LvalVar "$idx")
        AST.BinOpLess
        (arrLoc . AST.ExprLval $ AST.LvalField (arrLoc . AST.ExprLval $ AST.LvalVar "$arr") "length")

    whileBody = AST.StmtBlock
        [ arrLoc . AST.StmtDecl $ AST.LocalDecl ty
            [arrLoc . AST.LocalDeclItem idx $ Just (arrLoc . AST.ExprLval $
                AST.LvalArray
                    (arrLoc . AST.ExprLval $ AST.LvalVar "$arr")
                    (arrLoc . AST.ExprLval $ AST.LvalVar "$idx"))
            ]
        , body
        ]

    arrLoc = AST.Located (array ^. AST.loc)
    stLoc = AST.Located l

transStmt st@(AST.Located l (AST.StmtExpr expr)) = void $ transExpr (AST.Located l expr)
transStmt st@(AST.Located l (AST.StmtDecl decl)) = checkType st expectedType >> forM_ (decl ^. AST.localDeclItems) go
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
            (IStore (Store (memoryOfVariable $ var ^. varLoc) (sizeOf ty) operand))
            li [InstrComment $ "initialize" <+> pPrint item]
transStmt st@(AST.Located l AST.StmtNone) = pure ()

transExpr :: (HasCallStack, GIRMonad m) => AST.Located AST.Expr -> m (AST.Type, Operand)
transExpr   (AST.Located l (AST.ExprLval lval)) = do
    (ty, memory) <- transLval (AST.Located l lval)
    value <- emitInstr Nothing (ILoad (Load memory (sizeOf ty))) l [InstrComment $ "load" <+> pPrint lval]
    pure (ty, OperandNamed value)
transExpr    (AST.Located _ (AST.ExprInt i)) = pure (AST.TyInt, OperandInt i)
transExpr    (AST.Located l (AST.ExprString str)) = do
    name <- mkName Nothing
    internString name str
    value <- emitInstr Nothing (IGetAddr (GetAddr (MemoryGlobal (nameToIdent name)))) l [InstrComment . pPrint $ BS.unpack str]
    pure (AST.TyString, OperandNamed value)
transExpr    (AST.Located _ AST.ExprTrue) = pure (AST.TyBool, OperandInt 1)
transExpr    (AST.Located _ AST.ExprFalse) = pure (AST.TyBool, OperandInt 0)
transExpr    (AST.Located _ AST.ExprNull) = pure (AST.TyNull, OperandInt 0)
transExpr ex@(AST.Located l (AST.ExprCall funLval argExprs)) =
    transFunCall funLval >>= \case
        Nothing -> do
            simpleError ex $ "Unresolved function, unable to call"
            pure (AST.TyInt, OperandInt 0) -- TODO
        Just (dest, info) -> do
            checkCallArgumentsLength ex (info ^. funcInfoDecl . AST.obj . AST.funcArgs) argExprs
            args <- zipWithM prepareArg (info ^. funcInfoDecl . AST.obj . AST.funcArgs) argExprs
            ret <- emitInstr Nothing (ICall $ Call dest args) l [InstrComment $ "call" <+> pPrint ex]
            pure (info ^. funcInfoDecl . AST.obj . AST.funcRetType, OperandNamed ret)
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
    endBlock <- newBlock "andEnd"
    midBlock <- newBlock "andMid"
    block <- use girCurrentBlock

    operandLhs <- transExprTypeEqual AST.TyBool lhs
    setEnd $ BlockEndBranchCond operandLhs midBlock endBlock

    operandRhs <- inBlock midBlock $ do
        operandRhs <- transExprTypeEqual AST.TyBool rhs
        setEnd $ BlockEndBranch endBlock
        pure operandRhs

    switchToBlock endBlock
    value <- emitPhi (Just "and") [PhiBranch block operandLhs, PhiBranch midBlock operandRhs]
    pure (AST.TyBool, OperandNamed value)
transExpr (AST.Located _ (AST.ExprBinOp lhs AST.BinOpOr rhs)) = do
    endBlock <- newBlock "orEnd"
    midBlock <- newBlock "orMid"
    block <- use girCurrentBlock

    operandLhs <- transExprTypeEqual AST.TyBool lhs
    setEnd $ BlockEndBranchCond operandLhs endBlock midBlock

    operandRhs <- inBlock midBlock $ do
        operandRhs <- transExprTypeEqual AST.TyBool rhs
        setEnd $ BlockEndBranch endBlock
        pure operandRhs

    switchToBlock endBlock
    value <- emitPhi (Just "or") [PhiBranch block operandLhs, PhiBranch midBlock operandRhs]
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
        (AST.TyString, AST.BinOpPlus, AST.TyString) -> do
            val <- emitInstr (Just "concat") (IIntristic (IntristicConcat operandLhs operandRhs)) l []
            pure (AST.TyString, OperandNamed val)
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
transExpr ex@(AST.Located l (AST.ExprNew ty)) = do
    checkType ex ty
    case ty of
        AST.TyClass name -> do
            size <- view (girClasses . at name . singular _Just . classSize)
            val <- emitInstr Nothing (IIntristic (IntristicAlloc (OperandInt size) (ObjectClass $ coerce name)))
                    l [InstrComment $ pPrint ex]
            pure (ty, OperandNamed val)
        _ -> do
            simpleError ex $ "Not a class type:" <+> pPrint ty
            pure (AST.TyNull, OperandInt 0)
transExpr ex@(AST.Located l (AST.ExprNewArr ty lenExpr)) = do
    checkType ex ty
    lenOperand <- transExprTypeEqual AST.TyInt lenExpr
    sizeOperand <- OperandNamed <$> emitInstr Nothing (IBinOp (BinOp lenOperand BinOpTimes (OperandSize $ sizeOf ty))) l []
    val <- emitInstr Nothing (IIntristic (IntristicAlloc sizeOperand objectType)) l [InstrComment $ pPrint ex]
    pure (AST.TyArray ty, OperandNamed val)
  where
    objectType = case ty of
        AST.TyArray _ -> ObjectArray
        AST.TyClass _ -> ObjectArray
        _ -> ObjectPrimArray

transExpr ex@(AST.Located _ (AST.ExprCast targetTy expr)) = do
    (ty, operand) <- transExpr expr
    checkTypeImplicitConv ex targetTy ty
    pure (targetTy, operand)

transExprTypeEqual :: GIRMonad m => AST.Type -> AST.Located AST.Expr -> m Operand
transExprTypeEqual expectedType expr = do
    (ty, operand) <- transExpr expr
    checkTypeEqual expr expectedType ty
    pure operand

transLval :: (HasCallStack, GIRMonad m) => AST.Located AST.Lval -> m (AST.Type, Memory)
transLval lval@(AST.Located _ (AST.LvalVar ident)) =
    use (girVariables . at ident) >>= \case
        Nothing -> do
            simpleError lval $ "Unbound variable" <+> pPrint ident
            pure (AST.TyInt, MemoryLocal 0)
        Just var -> do
            case var ^. varState of
                VarUndefined -> simpleError lval $ "Variable referenced before definition"
                VarUsable -> pure ()
            pure (var ^. varType, memoryOfVariable $ var ^. varLoc)
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

transFunCall :: (HasCallStack, GIRMonad m) => AST.Located AST.Lval -> m (Maybe (CallDest, FuncInfo))
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

transFuncDecl :: (HasCallStack, GIRMonad m) => AST.Located AST.FuncDecl -> m ()
transFuncDecl locDecl@(AST.Located l decl) = do
    entryBlock <- newBlock "entry"

    girVariableCnt .= 0
    girVariables .= Map.empty
    girCurrentBlock .= entryBlock

    local (girCurrentFunction ?~ decl) $ do
        zipWithM_ addArg [0..] (decl ^. AST.funcArgs)
        forM_ (decl ^. AST.funcArgs) markAsUsable
        transStmt $ decl ^. AST.funcBody
        when (decl ^. AST.funcRetType == AST.TyVoid) $
            setEnd BlockEndReturnVoid

    addFunction (coerce $ decl ^. AST.funcName) entryBlock
  where
    addArg idx arg = do
        checkType locDecl (arg ^. AST.funArgType)
        use (girVariables . at (arg ^. AST.funArgName)) >>= \case
            Just var | var ^. varState == VarUndefined ->
                simpleError locDecl $ "Duplicate definition of argument:" <+> pPrint (arg ^. AST.funArgName)
            _ -> pure ()

        girVariables . at (arg ^. AST.funArgName) ?= Variable
            { _varType = arg ^. AST.funArgType
            , _varLoc = VariableArgument idx
            , _varState = VarUndefined
            , _varDefinedIn = Nothing
            }

    markAsUsable arg = girVariables . at (arg ^. AST.funArgName) . singular _Just . varState .= VarUsable

transClassDecl :: GIRMonad m => AST.Located AST.ClassDecl -> m ()
transClassDecl = undefined -- TODO

transProgram :: (HasCallStack, GIRMonad m) => AST.Program -> m ()
transProgram (AST.Program prog) = do
    functionsMap <- view girFunctions
    functionsMap <- foldM addFunction functionsMap functions
    local (girFunctions .~ functionsMap) $ forM_ prog $ \case
        AST.Located l (AST.TLDFunc decl) -> transFuncDecl $ AST.Located l decl
        AST.Located l (AST.TLDClass decl) -> transClassDecl $ AST.Located l decl
    case Map.lookup "main" functionsMap of
        Just info -> do
            let decl = info ^. funcInfoDecl
            unless (length (decl ^. AST.obj . AST.funcArgs) == 0) $
                simpleError decl "main must be parameterless"
            unless (decl ^. AST.obj . AST.funcRetType == AST.TyInt) $
                simpleError decl "main must return int"
        Nothing -> report Diagnostic
            { _diagWhere = Nothing
            , _diagContent = "no definition of main"
            , _diagType = DiagnosticError
            , _diagNotes = []
            }
    pure ()
  where
    functions = [ AST.Located l decl | AST.Located l (AST.TLDFunc decl) <- prog ]
    addFunction acc decl
        | Map.member name acc = do
            simpleError decl $ "Function redefinition"
            pure acc
        | otherwise = pure $ Map.insert name (FuncInfo decl Nothing) acc
      where
        name = decl ^. AST.obj . AST.funcName

memoryOfVariable :: VariableLoc -> Memory
memoryOfVariable (VariableLocal idx) = MemoryLocal idx
memoryOfVariable (VariableArgument idx) = MemoryArgument idx

checkType :: (GIRMonad m, Reportible r) => r -> AST.Type -> m ()
checkType ctx AST.TyInt = pure ()
checkType ctx AST.TyBool = pure ()
checkType ctx AST.TyVoid = pure ()
checkType ctx AST.TyString = pure ()
checkType ctx AST.TyNull = pure ()
checkType ctx (AST.TyArray ty) = checkType ctx ty
checkType ctx (AST.TyClass name) =
    view (girClasses . at name) >>= \case
        Just _ -> pure ()
        Nothing -> simpleError ctx $ "Unknown class" <+> pPrint name

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
    ioref <- use $ girCurrentBlock . blockBody
    liftIO $ modifyIORef' ioref (Seq.|> Instruction result payload (Just loc) meta)
    pure result

emitPhi :: GIRMonad m => Maybe Ident -> [PhiBranch] -> m Name
emitPhi humanName branches = do
    result <- mkName humanName
    ioref <- use $ girCurrentBlock . blockPhi
    liftIO $ modifyIORef' ioref (PhiNode result branches :)
    pure result

setEnd :: GIRMonad m => BlockEnd -> m ()
setEnd end = whenReachable $ do
    ioref <- use $ girCurrentBlock . blockEnd
    liftIO $ writeIORef ioref end

sizeOf :: AST.Type -> Size
sizeOf AST.TyInt = Size32
sizeOf AST.TyBool = Size8
sizeOf AST.TyVoid = Size0
sizeOf AST.TyString = SizePtr
sizeOf (AST.TyArray _) = SizePtr
sizeOf (AST.TyClass _) = SizePtr
sizeOf AST.TyNull = SizePtr

generateIR :: AST.Program -> MEMonad ()
generateIR program = do
    me <- get
    ret <- execStateT (runReaderT (transProgram program) env0) GIRState
        { _girME = me
        , _girVariableCnt = 0
        , _girVariables = Map.empty
        , _girCurrentBlock = error "Outside"
        }
    put (ret ^. girME)
  where
    env0 = GIREnv
        { _girCurrentFunction = Nothing
        , _girCurrentClass = Nothing
        , _girFunctions =
            [ builtin "printInt" AST.TyVoid [AST.TyInt]
            , builtin "printString" AST.TyVoid [AST.TyString]
            , builtin "error" AST.TyVoid []
            , builtin "readInt" AST.TyInt []
            , builtin "readString" AST.TyString []
            ]
        , _girClasses = Map.empty
        }
    nowhere = AST.LocRange (AST.Location 0 0) (AST.Location 0 0)
    builtin name retTy argTys = (name, flip FuncInfo Nothing $ AST.Located nowhere AST.FuncDecl
        { AST._funcName = name
        , AST._funcArgs = [ AST.FunArg ty "" | ty <- argTys ]
        , AST._funcRetType = retTy
        , AST._funcBody = AST.Located nowhere AST.StmtNone
        })
