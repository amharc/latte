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
module Language.Latte.Frontend.GenIR where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce
import Data.IORef
import Data.Foldable
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
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
    , _girBlockVariables :: Map.Map AST.Ident Variable
    , _girCurrentBlock :: Block
    }

data GIREnv = GIREnv
    { _girCurrentFunction :: Maybe AST.FuncDecl
    , _girCurrentClass :: Maybe ClassInfo
    , _girFunctions :: Map.Map AST.Ident FuncInfo
    , _girClasses :: Map.Map AST.Ident ClassInfo
    }

data FuncInfo = FuncInfo
    { _funcInfoDecl :: AST.Located AST.FuncDecl
    , _funcInfoVtablePos :: Maybe Int
    }

data ClassInfo = ClassInfo
    { _classInfoDecl :: AST.Located AST.ClassDecl
    , _classFields :: Map.Map AST.Ident ClassField
    , _classMethods :: Map.Map AST.Ident FuncInfo
    , _classBase :: Maybe ClassInfo
    , _classObjFields :: Seq.Seq AST.Type
    , _classObjVtable :: Seq.Seq Ident
    }

data ClassField = ClassField
    { _classFieldType :: AST.Type
    , _classFieldId :: Int
    }

type GIRMonad m = (MonadState GIRState m, MonadReader GIREnv m, MonadIO m, HasCallStack)

data Variable = Variable
    { _varType :: !AST.Type
    , _varLoc :: VariableLoc
    }

data VariableLoc = VariableLocal !Int | VariableArgument !Int

makeLenses ''GIRState
makeLenses ''GIREnv
makeLenses ''FuncInfo
makeLenses ''ClassInfo
makeLenses ''ClassField
makeLenses ''Variable

instance HasMiddleEndState GIRState where
    middleEndState = girME

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

transStmt :: GIRMonad m => AST.Located AST.Stmt -> m ()
transStmt (AST.Located _ (AST.StmtBlock stmts)) = do
    oldVariables <- use girVariables
    girBlockVariables .= Map.empty

    forM_ stmts $ \case
        AST.Located l (AST.StmtDecl decl) -> addDecl (AST.Located l decl)
        _ -> pure ()

    forM_ stmts transStmt

    girVariables .= oldVariables
  where
    addDecl decl = forM_ (decl ^. AST.obj ^. AST.localDeclItems) $ \item -> do
        ident <- girVariableCnt <<+= 1

        use (girBlockVariables . at (item ^. AST.obj ^. AST.localDeclName)) >>= \case
            Just var ->
                simpleError item $ "Duplicate definition of variable"
            _ -> pure ()

        girBlockVariables . at (item ^. AST.obj ^. AST.localDeclName) ?= Variable
            { _varType = decl ^. AST.obj ^. AST.localDeclType
            , _varLoc = VariableLocal ident
            }
transStmt st@(AST.Located l (AST.StmtAssign lval expr)) = do
    (lvalTy, lvalMem) <- transLval lval
    (exprTy, exprOperand) <- transExpr expr
    checkTypeImplicitConv st lvalTy exprTy
    void $ emitInstr Nothing (Store lvalMem (sizeOf lvalTy) exprOperand) l [InstrComment $ pPrint st]
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
    void $ emitInstr Nothing (Inc memory (sizeOf ty)) l [InstrComment $ "increment of " <+> pPrint lval]
transStmt st@(AST.Located l (AST.StmtDec lval)) = do
    (ty, memory) <- transLval lval
    checkTypeEqual st AST.TyInt ty
    void $ emitInstr Nothing (Dec memory (sizeOf ty)) l [InstrComment $ "decrement of " <+> pPrint lval]
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
        (ty, operand) <- case item ^. AST.localDeclValue of
            Just expr -> transExpr expr
            Nothing -> case expectedType of
                AST.TyString -> do
                    emptyStr <- emitInstr (Just "emptyString") (GetAddr $ MemoryGlobal "LATC_emptyString") li []
                    pure (AST.TyString, Operand (OperandNamed emptyStr) SizePtr)
                _ -> pure (expectedType, Operand (OperandInt 0) (sizeOf expectedType))
        var <- use $ girBlockVariables . at (item ^. AST.localDeclName) . singular _Just
        girVariables . at (item ^. AST.localDeclName) ?= var
        checkTypeImplicitConv locItem expectedType ty
        void $ emitInstr Nothing
            (Store (memoryOfVariable $ var ^. varLoc) (sizeOf ty) operand)
            li [InstrComment $ "initialize" <+> pPrint item]
transStmt st@(AST.Located l AST.StmtNone) = pure ()

transExpr :: GIRMonad m => AST.Located AST.Expr -> m (AST.Type, Operand)
transExpr   (AST.Located l (AST.ExprLval lval)) = do
    (ty, memory) <- transLval (AST.Located l lval)
    value <- emitInstr Nothing (Load memory (sizeOf ty)) l [InstrComment $ "load" <+> pPrint lval]
    pure (ty, Operand (OperandNamed value) (sizeOf ty))
transExpr    (AST.Located _ (AST.ExprInt i)) = pure (AST.TyInt, Operand (OperandInt i) Size32)
transExpr    (AST.Located l (AST.ExprString str)) = do
    name <- mkName Nothing
    internString (mangleString name) str
    value <- emitInstr Nothing (GetAddr . MemoryGlobal $ mangleString name) l [InstrComment . pPrint $ BS.unpack str]
    pure (AST.TyString, Operand (OperandNamed value) SizePtr)
transExpr    (AST.Located _ AST.ExprTrue) = pure (AST.TyBool, Operand (OperandInt 1) Size8)
transExpr    (AST.Located _ AST.ExprFalse) = pure (AST.TyBool, Operand (OperandInt 0) Size8)
transExpr    (AST.Located _ AST.ExprNull) = pure (AST.TyNull, Operand (OperandInt 0) SizePtr)
transExpr ex@(AST.Located l (AST.ExprCall funLval argExprs)) =
    transFunCall funLval >>= \case
        Nothing -> do
            simpleError ex $ "Unresolved function, unable to call"
            pure (AST.TyInt, Operand OperandUndef SizePtr)
        Just (dest, arg, info) -> do
            checkCallArgumentsLength ex (info ^. funcInfoDecl . AST.obj . AST.funcArgs) argExprs
            args <- maybe id (:) arg <$> zipWithM prepareArg (info ^. funcInfoDecl . AST.obj . AST.funcArgs) argExprs
            ret <- emitInstr Nothing (Call dest args) l [InstrComment $ "call" <+> pPrint ex]
            let retTy = info ^. funcInfoDecl . AST.obj . AST.funcRetType
            pure (retTy, Operand (OperandNamed ret) (sizeOf retTy))
  where
     prepareArg :: GIRMonad m => AST.Located AST.FunArg -> AST.Located AST.Expr -> m Operand
     prepareArg (AST.Located _ arg) expr = do
        (ty, operand) <- transExpr expr
        checkTypeImplicitConv expr (arg ^. AST.funArgType) ty 
        pure operand
transExpr ex@(AST.Located l (AST.ExprUnOp AST.UnOpNot arg)) = do
    operand <- transExprTypeEqual AST.TyBool arg
    ret <- emitInstr (Just "not") (UnOp UnOpNot operand) l []
    pure (AST.TyBool, Operand (OperandNamed ret) (sizeOf AST.TyBool))
transExpr ex@(AST.Located l (AST.ExprUnOp AST.UnOpNeg arg)) = do
    operand <- transExprTypeEqual AST.TyInt arg
    ret <- emitInstr (Just "neg") (UnOp UnOpNeg operand) l []
    pure (AST.TyInt, Operand (OperandNamed ret) (sizeOf AST.TyInt))
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
    pure (AST.TyBool, Operand (OperandNamed value) (sizeOf AST.TyBool))
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
    pure (AST.TyBool, Operand (OperandNamed value) (sizeOf AST.TyBool))
transExpr ex@(AST.Located l (AST.ExprBinOp lhs op rhs)) = do
    (tyLhs, operandLhs) <- transExpr lhs
    (tyRhs, operandRhs) <- transExpr rhs

    let emit op ty = flip Operand (sizeOf ty) . OperandNamed <$> 
            emitInstr Nothing (BinOp operandLhs op operandRhs) l [InstrComment $ pPrint ex]

    case (tyLhs, op, tyRhs) of
        (_, AST.BinOpEqual, _) -> do
            checkTypesComparable ex tyLhs tyRhs
            (AST.TyBool,) <$> emit BinOpEqual AST.TyBool
        (_, AST.BinOpNotEqual, _) -> do
            checkTypesComparable ex tyLhs tyRhs
            (AST.TyBool,) <$> emit BinOpNotEqual AST.TyBool
        (AST.TyString, AST.BinOpPlus, AST.TyString) -> do
            val <- emitInstr (Just "concat") (IIntristic (IntristicConcat operandLhs operandRhs)) l []
            pure (AST.TyString, Operand (OperandNamed val) (sizeOf AST.TyString))
        (AST.TyInt, intOp -> Just (retTy, op), AST.TyInt) ->
            (retTy,) <$> emit op retTy
        _ -> do
            simpleError ex "Invalid binary operator application"
            pure (AST.TyNull, Operand OperandUndef SizePtr)
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
            val <- emitInstr Nothing (IIntristic (IntristicClone (MemoryGlobal (mangleClassPrototype name))))
                    l [InstrComment $ pPrint ex]
            pure (ty, Operand (OperandNamed val) (sizeOf ty))
        _ -> do
            simpleError ex $ "Not a class type:" <+> pPrint ty
            pure (AST.TyNull, Operand OperandUndef SizePtr)
transExpr ex@(AST.Located l (AST.ExprNewArr ty lenExpr)) = do
    checkType ex ty
    lenOperand <- transExprTypeEqual AST.TyInt lenExpr
    sizeOperand <- flip Operand (sizeOf AST.TyInt) . OperandNamed <$> emitInstr Nothing (BinOp lenOperand BinOpTimes (Operand (OperandSize $ sizeOf ty) (sizeOf AST.TyInt))) l []
    val <- emitInstr Nothing (IIntristic (IntristicAlloc sizeOperand objectType)) l [InstrComment $ pPrint ex]
    pure (AST.TyArray ty, Operand (OperandNamed val) SizePtr)
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

transLval :: GIRMonad m => AST.Located AST.Lval -> m (AST.Type, Memory)
transLval lval@(AST.Located l (AST.LvalVar ident)) =
    use (girVariables . at ident) >>= \case
        Nothing -> views girCurrentClass (>>= view (classFields . at ident)) >>= \case
            Nothing -> do
                simpleError lval $ "Unbound variable" <+> pPrint ident
                pure (AST.TyInt, MemoryUndef)
            Just field -> do
                this <- flip Operand SizePtr . OperandNamed <$> emitInstr Nothing (Load (MemoryArgument 0) SizePtr) l []
                pure (field ^. classFieldType, MemoryOffset this (Operand (OperandInt $ field ^. classFieldId) Size32) SizePtr)
        Just var -> do
            pure (var ^. varType, memoryOfVariable $ var ^. varLoc)
transLval lval@(AST.Located l (AST.LvalArray arrExpr idxExpr)) = do
    (tyArr, operandArr) <- transExpr arrExpr
    operandIdx <- transExprTypeEqual AST.TyInt idxExpr
    case tyArr of
        AST.TyArray ty -> do
            arrData <- flip Operand SizePtr . OperandNamed <$> emitInstr Nothing
                (GetAddr $ MemoryOffset operandArr (Operand (OperandInt 1) Size32) SizePtr) l [InstrComment "skip length field"]
            pure (ty, MemoryOffset arrData operandIdx (sizeOf ty))
        ty -> do
            simpleError lval $ "Not an array: " <+> pPrint ty
            pure (AST.TyInt, MemoryUndef)
transLval lval@(AST.Located _ (AST.LvalField objExpr field)) = do
    (tyObj, operandObj) <- transExpr objExpr
    case tyObj of
        AST.TyClass className ->
            view (girClasses . at className . singular _Just . classFields . at field) >>= \case
                Nothing -> do
                    simpleError lval $ pPrint className <+> "has no field" <+> pPrint field
                    pure (AST.TyInt, MemoryLocal 0)
                Just field ->
                    pure (field ^. classFieldType, MemoryOffset operandObj (Operand (OperandInt $ field ^. classFieldId) Size32) SizePtr)
        AST.TyString -> do
            unless (field == "length") . simpleError lval $ "string has no field" <+> pPrint field
            pure $ lengthField operandObj
        AST.TyArray _ -> do
            unless (field == "length") . simpleError lval $ pPrint tyObj <+> "has no field" <+> pPrint field
            pure $ lengthField operandObj
        _ -> do
            simpleError lval $ pPrint tyObj <+> "has no fields"
            pure $ lengthField operandObj
  where
    lengthField operandObj = (AST.TyInt, MemoryOffset operandObj (Operand (OperandInt 0) Size32) SizePtr)

transFunCall :: GIRMonad m => AST.Located AST.Lval -> m (Maybe (Memory, Maybe Operand, FuncInfo))
transFunCall (AST.Located l (AST.LvalVar name)) = view (girFunctions . at name) >>= \case
    Nothing -> pure Nothing
    Just info -> case info ^. funcInfoVtablePos of
        Nothing -> pure $ Just (MemoryGlobal $ mangle Nothing name, Nothing, info)
        Just offset -> do
            this <- flip Operand SizePtr . OperandNamed <$> emitInstr (Just "this") (Load (MemoryArgument 0) SizePtr) l []
            addr <- virtualFuncAddr l this offset
            pure $ Just (addr, Just this, info)
transFunCall lval@(AST.Located l (AST.LvalField objExpr name)) = do
    (objTy, objOperand) <- transExpr objExpr
    case objTy of
        AST.TyClass className ->
            view (girClasses . at className . singular _Just . classMethods . at name) >>= \case
                Nothing -> do
                    simpleError lval $ pPrint className <+> "has no method" <+> pPrint name
                    pure Nothing
                Just info -> do
                    addr <- virtualFuncAddr l objOperand (info ^. funcInfoVtablePos . singular _Just)
                    pure $ Just (addr, Just objOperand, info)
        _ -> do
            simpleError lval $ "not a class"
            pure Nothing
transFunCall lval@(AST.Located _ (AST.LvalArray _ _)) = do
    simpleError lval "Not a function"
    pure Nothing

virtualFuncAddr :: GIRMonad m => AST.LocRange -> Operand -> Int -> m Memory
virtualFuncAddr l obj idx = do
    vtablePtr <- flip Operand SizePtr . OperandNamed <$> emitInstr (Just "vtable") (Load (MemoryOffset obj (Operand (OperandInt 0) Size32) SizePtr) SizePtr)
        l [InstrComment "load vtable ptr", InstrInvariant]
    pure $ MemoryOffset vtablePtr (Operand (OperandInt idx) Size32) SizePtr

transFuncDecl :: GIRMonad m => Maybe AST.Ident -> AST.Located AST.FuncDecl -> m ()
transFuncDecl mClass locDecl@(AST.Located l decl) = do
    entryBlock <- newBlock "entry"

    girVariableCnt .= 0
    girVariables .= Map.empty
    girCurrentBlock .= entryBlock

    local (girCurrentFunction ?~ decl) $ do
        zipWithM_ addArg [argStart..] (decl ^. AST.funcArgs)
        transStmt $ decl ^. AST.funcBody
        when (decl ^. AST.funcRetType == AST.TyVoid) $
            setEnd BlockEndReturnVoid

    addFunction (mangle mClass (decl ^. AST.funcName)) entryBlock
  where
    addArg idx arg = do
        checkType arg (arg ^. AST.obj . AST.funArgType)
        use (girVariables . at (arg ^. AST.obj . AST.funArgName)) >>= \case
            Just var ->
                simpleError locDecl $ "Duplicate definition of argument:" <+> pPrint (arg ^. AST.obj . AST.funArgName)
            _ -> pure ()

        girVariables . at (arg ^. AST.obj . AST.funArgName) ?= Variable
            { _varType = arg ^. AST.obj . AST.funArgType
            , _varLoc = VariableArgument idx
            }

    argStart = case mClass of
        Just _ -> 1
        Nothing -> 0

transClassDecl :: GIRMonad m => AST.Located AST.ClassDecl -> m ()
transClassDecl decl = do
    info <- view $ girClasses . at (decl ^. AST.obj . AST.className) . singular _Just
    local (girCurrentClass ?~ info) $ forM_ (decl ^. AST.obj . AST.classMembers) $ \case
        l@(AST.Located _ (AST.ClassMemberField field)) ->
            checkType l (field ^. AST.classFieldType)
        AST.Located l (AST.ClassMemberMethod func) ->
            transFuncDecl (Just name) (AST.Located l func)

    emitVtable info
    emitPrototype info
  where
    name = decl ^. AST.obj . AST.className

emitVtable :: GIRMonad m => ClassInfo -> m ()
emitVtable info = internObject (mangleClassVtable name) vtable
  where
    name = info ^. classInfoDecl . AST.obj . AST.className
    vtable = Object [ObjectField (ObjectFieldRef ident) | ident <- toList $ info ^. classObjVtable]

emitPrototype :: GIRMonad m => ClassInfo -> m ()
emitPrototype info = internObject (mangleClassPrototype name) prototype
  where
    name = info ^. classInfoDecl . AST.obj . AST.className
    prototype = Object $ ObjectField (ObjectFieldRef (mangleClassVtable name)) : fields
    fields = [ObjectField (defaultValue field) | field <- toList $ info ^. classObjFields]

    defaultValue AST.TyInt = ObjectFieldInt 0
    defaultValue AST.TyBool = ObjectFieldInt 0
    defaultValue AST.TyVoid = ObjectFieldInt 0
    defaultValue (AST.TyArray _) = ObjectFieldNull
    defaultValue (AST.TyClass _) = ObjectFieldNull
    defaultValue AST.TyNull = ObjectFieldNull
    defaultValue AST.TyString = ObjectFieldRef "LATC_emptyString"

transProgram :: GIRMonad m => AST.Program -> m ()
transProgram (AST.Program prog) = do
    functionsMap' <- view girFunctions
    functionsMap <- foldM addFunction functionsMap' functions

    checkMain functionsMap

    classMap' <- foldM addClass Map.empty classes
    classMap <- foldM (updateClass classMap' Set.empty) Map.empty classes

    local (girFunctions .~ functionsMap) $ local (girClasses .~ classMap) $ forM_ prog $ \case
        AST.Located l (AST.TLDFunc decl) -> transFuncDecl Nothing $ AST.Located l decl
        AST.Located l (AST.TLDClass decl) -> transClassDecl $ AST.Located l decl
  where
    functions = [ AST.Located l decl | AST.Located l (AST.TLDFunc decl) <- prog ]
    classes = [ AST.Located l decl | AST.Located l (AST.TLDClass decl) <- prog ]

    addFunction acc decl
        | Map.member name acc = do
            simpleError decl "Function redefinition"
            pure acc
        | otherwise = pure $ Map.insert name (FuncInfo decl Nothing) acc
      where
        name = decl ^. AST.obj . AST.funcName

    addClass acc decl
        | Map.member name acc = do
            simpleError decl "Class redefiniton"
            pure acc
        | otherwise = pure $ Map.insert name decl acc
      where
        name = decl ^. AST.obj . AST.className
    
    updateClass :: GIRMonad m => Map.Map AST.Ident (AST.Located AST.ClassDecl) -> Set.Set AST.Ident
        -> Map.Map AST.Ident ClassInfo -> AST.Located AST.ClassDecl -> m (Map.Map AST.Ident ClassInfo)
    updateClass decls stack acc decl
        | Set.member name stack = do
            simpleError decl "Cyclic class hierarchy"
            pure acc
        | Map.member name acc = pure acc
        | otherwise = case mbase of
            Nothing ->
                addSelf Nothing acc
            Just baseName | Just baseInfo <- Map.lookup baseName acc ->
                addSelf (Just baseInfo) acc
            Just baseName -> case Map.lookup baseName decls of
                Nothing -> do
                    simpleError decl $ "Unknown base class" <+> pPrint baseName
                    addSelf Nothing acc
                Just baseDecl -> do
                    acc <- updateClass decls (Set.insert name stack) acc baseDecl
                    case Map.lookup baseName acc of
                        Just baseInfo -> addSelf (Just baseInfo) acc
                        Nothing -> fail "should not happen"
      where
        name = decl ^. AST.obj . AST.className
        mbase = decl ^. AST.obj . AST.classBase
        fields = [AST.Located l field | AST.Located l (AST.ClassMemberField field) <- decl ^. AST.obj . AST.classMembers]
        methods = [AST.Located l func | AST.Located l (AST.ClassMemberMethod func) <- decl ^. AST.obj . AST.classMembers]

        addSelf mbaseInfo acc = do
            info <- computeSelf mbaseInfo acc
            pure $ Map.insert name info acc

        computeSelf mbaseInfo acc = do
            let baseFields = maybe Map.empty (view classFields) mbaseInfo
                baseMethods = maybe Map.empty (view classMethods) mbaseInfo
                baseObjFields = maybe Seq.empty (view classObjFields) mbaseInfo
                baseObjVtable = maybe Seq.empty (view classObjVtable) mbaseInfo
            (fields, objFields) <- foldM (addField baseFields) (Map.empty, baseObjFields) fields
            (methods, objVtable) <- foldM (addMethod baseMethods) (Map.empty, baseObjVtable) methods
            pure $ ClassInfo
                { _classInfoDecl = decl
                , _classFields = Map.union fields baseFields
                , _classMethods = Map.union methods baseMethods
                , _classObjFields = objFields
                , _classObjVtable = objVtable
                , _classBase = mbaseInfo
                }

        addField baseFields (acc, objFields) field
            | Map.member name acc = do
                simpleError field "Duplicate field declaration"
                pure (acc, objFields)
            | otherwise =
                pure (Map.insert name (ClassField ty (1 + Seq.length objFields)) acc, objFields Seq.|> ty)
          where
            ty = field ^. AST.obj . AST.classFieldType
            name = field ^. AST.obj . AST.classFieldName

        addMethod baseMethods (acc, objVtable) method
            | Map.member name acc = do
                simpleError method "Method redefinition"
                pure (acc, objVtable)
            | Just baseMethod <- Map.lookup name baseMethods = do
                checkOverride (baseMethod ^. funcInfoDecl) method
                let pos = baseMethod ^. funcInfoVtablePos . singular _Just
                    objVtable' = Seq.update pos mangled objVtable
                pure (Map.insert name (FuncInfo method (Just pos)) acc, objVtable')
            | otherwise =
                let pos = Seq.length objVtable in
                pure (Map.insert name (FuncInfo method (Just pos)) acc, objVtable Seq.|> mangled)
          where
            name = method ^. AST.obj . AST.funcName
            mangled = mangle (Just $ decl ^. AST.obj . AST.className) name

    checkMain functionsMap = case Map.lookup "main" functionsMap of
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

checkOverride :: GIRMonad m => AST.Located AST.FuncDecl -> AST.Located AST.FuncDecl -> m ()
checkOverride base derived = do
    unless (length baseArgs == length derivedArgs) $
        simpleError derived "Number of arguments does not match base class"
    zipWithM_ checkArg baseArgs derivedArgs
    checkTypeImplicitConv derived (base ^. AST.obj . AST.funcRetType) (derived ^. AST.obj . AST.funcRetType)
  where
    baseArgs = base ^. AST.obj . AST.funcArgs
    derivedArgs = derived ^. AST.obj . AST.funcArgs

    checkArg baseArg derivedArg =
        checkTypeImplicitConv derivedArg (derivedArg ^. AST.obj . AST.funArgType) (baseArg ^. AST.obj . AST.funArgType)
    
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
    , _diagContent = head $+$ nest 4 (pPrint ctx)
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
checkTypeImplicitConv ctx expected got = case (expected, got) of
    (AST.TyClass _, AST.TyNull) -> pure ()
    (AST.TyClass c, AST.TyClass c') -> do
        info <- view (girClasses . at c)
        info' <- view (girClasses . at c')
        case (info, info') of
            (Just info, Just info') ->  unless (name info `elem` map name (classBases info')) err
            _ -> simpleError ctx "Type error"
    (t, t') | t == t' -> pure ()
    _ -> err
  where
    err = simpleError ctx $ hsep
        [ "Type mismatch: cannot convert"
        , pPrint got
        , "to:"
        , pPrint expected
        ]

    name info = info ^. classInfoDecl . AST.obj . AST.className
    
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

classBases :: ClassInfo -> [ClassInfo]
classBases cls = case cls ^. classBase of
    Nothing -> [cls]
    Just base -> cls : classBases base

emitInstr :: GIRMonad m => Maybe Ident -> InstrPayload -> AST.LocRange -> [InstrMetadata] -> m Name
emitInstr humanName payload loc meta = do
    result <- mkName humanName
    ioref <- use $ girCurrentBlock . blockBody
    liftIO $ modifyIORef' ioref (Seq.|> Instruction (Just result) payload (InstrLocation loc : meta))
    pure result

emitPhi :: GIRMonad m => Maybe Ident -> [PhiBranch] -> m Name
emitPhi humanName branches = do
    result <- mkName humanName
    ioref <- use $ girCurrentBlock . blockPhi
    liftIO $ modifyIORef' ioref (PhiNode result branches Seq.<|)
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
        , _girBlockVariables = Map.empty
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
        , AST._funcArgs = [ AST.Located nowhere $ AST.FunArg ty "" | ty <- argTys ]
        , AST._funcRetType = retTy
        , AST._funcBody = AST.Located nowhere AST.StmtNone
        })

mangle :: Maybe AST.Ident -> AST.Ident -> Ident
mangle Nothing funName = coerce $ BS.concat ["_function", manglePart funName]
mangle (Just className) funName = coerce $ BS.concat
    [ "_method"
    , manglePart className
    , manglePart funName
    ]

manglePart :: AST.Ident -> BS.ByteString
manglePart part = BS.concat ["_", BS.pack . show . BS.length $ coerce part, "_", coerce part]

mangleClassPrototype :: AST.Ident -> Ident
mangleClassPrototype name = coerce $ BS.concat ["_proto", manglePart name]

mangleClassVtable :: AST.Ident -> Ident
mangleClassVtable name = coerce $ BS.concat ["_vtable", manglePart name]

mangleString :: Name -> Ident
mangleString name = coerce $ BS.concat ["_string", BS.pack . show . getUniqueId $ name ^. nameUnique]
