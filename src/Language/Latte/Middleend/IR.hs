{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.IR where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Function
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.String
import qualified Language.Latte.Frontend.AST as Frontend
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

newtype UniqueId = UniqueId { getUniqueId :: Int }
    deriving (Eq, Ord, Show)

newtype Ident = Ident { getIdent :: BS.ByteString }
    deriving (Eq, Ord, Show, IsString)

data Object = Object
    { _objectFields :: [ObjectField]
    }

data ObjectField = ObjectField
    { _objectFieldData :: ObjectFieldData
    }

data ObjectFieldData
    = ObjectFieldInt Int
    | ObjectFieldNull
    | ObjectFieldRef Ident

data Operand = Operand
    { _operandPayload :: OperandPayload
    , _operandSize :: Size
    }
    deriving (Eq, Ord, Show)

data OperandPayload
    = OperandNamed Name
    | OperandSize Size
    | OperandInt Int
    | OperandUndef
    deriving (Eq, Ord, Show)

data Name = Name
    { _nameUnique :: {-# UNPACK #-} UniqueId
    , _nameHuman :: (Maybe Ident)
    }
    deriving (Eq, Ord, Show)

data Memory
    = MemoryLocal {-# UNPACK #-} Int
    | MemoryArgument {-# UNPACK #-} Int
    | MemoryOffset Operand Operand Size Int
    | MemoryGlobal Ident
    | MemoryUndef
    deriving (Eq, Show)

pattern MemoryPointer :: Operand -> Memory
pattern MemoryPointer ptr = MemoryOffset ptr (Operand (OperandInt 0) SizePtr) SizePtr 0

data Size = Size0 | Size8 | Size32 | Size64 | SizePtr
    deriving (Eq, Ord, Show)

data InstrPayload
    = ILoad Load
    | IStore Store
    | IBinOp BinOp
    | IUnOp UnOp
    | IGetAddr GetAddr
    | ICall Call
    | IIntristic Intristic
    | IIncDec IncDec
    | IConst Operand
    deriving (Eq, Show)

data Instruction = Instruction
    { _instrResult :: (Maybe Name)
    , _instrPayload :: InstrPayload
    , _instrMetadata :: [InstrMetadata]
    }
    deriving (Eq, Show)

data InstrMetadata
    = InstrComment Doc
    | InstrInvariant
    | InstrLocation Frontend.LocRange
    deriving (Eq, Show)

data Block = Block
    { _blockName :: Name
    , _blockPhi :: {-# UNPACK #-} (IORef (Seq.Seq PhiNode))
    , _blockBody :: {-# UNPACK #-} (IORef (Seq.Seq Instruction))
    , _blockEnd :: {-# UNPACK #-} (IORef BlockEnd)
    }
    deriving Eq

data PhiNode = PhiNode
    { _phiName :: Name
    , _phiBranches :: [PhiBranch]
    }
    deriving Eq

data PhiBranch = PhiBranch
    { _phiFrom :: Block
    , _phiValue :: Operand
    }
    deriving Eq

data BlockEnd
    = BlockEndBranch Block
    | BlockEndBranchCond Operand Block Block
    | BlockEndReturn Operand
    | BlockEndReturnVoid
    | BlockEndNone
    deriving Eq

data Load = SLoad
    { _loadFrom :: Memory
    , _loadSize :: Size
    }
    deriving (Eq, Show)

pattern Load :: Memory -> Size -> InstrPayload
pattern Load from size = ILoad (SLoad from size)

data Store = SStore
    { _storeTo :: Memory
    , _storeSize :: Size
    , _storeValue :: Operand
    }
    deriving (Eq, Show)

pattern Store :: Memory -> Size -> Operand -> InstrPayload
pattern Store to size value = IStore (SStore to size value)

data BinOp = SBinOp
    { _binOpLhs :: Operand
    , _binOpOp ::  BinOperator
    , _binOpRhs :: Operand
    }
    deriving (Eq, Show)

pattern BinOp :: Operand -> BinOperator -> Operand -> InstrPayload
pattern BinOp lhs op rhs = IBinOp (SBinOp lhs op rhs)

data UnOp = SUnOp
    { _unOpOp :: UnOperator
    , _unOpArg :: Operand
    }
    deriving (Eq, Show)

pattern UnOp :: UnOperator -> Operand -> InstrPayload
pattern UnOp op arg = IUnOp (SUnOp op arg)

newtype GetAddr = SGetAddr { _getAddrMem :: Memory }
    deriving (Eq, Show)

pattern GetAddr :: Memory -> InstrPayload
pattern GetAddr mem = IGetAddr (SGetAddr mem)

data Call = SCall
    { _callDest :: Memory
    , _callArgs :: [Operand]
    }
    deriving (Eq, Show)

pattern Call :: Memory -> [Operand] -> InstrPayload
pattern Call dest args = ICall (SCall dest args)

data BinOperator
    = BinOpPlus
    | BinOpMinus
    | BinOpTimes
    | BinOpDivide
    | BinOpModulo
    | BinOpShiftLeft
    | BinOpShiftRight
    | BinOpLess
    | BinOpLessEqual
    | BinOpGreater
    | BinOpGreaterEqual
    | BinOpEqual
    | BinOpNotEqual
    | BinOpAnd
    | BinOpOr
    deriving (Eq, Show)

data UnOperator
    = UnOpNeg
    | UnOpNot
    deriving (Eq, Show)

data Intristic
    = IntristicAlloc Operand ObjectType
    | IntristicClone Operand Int
    | IntristicConcat Operand Operand
    deriving (Eq, Show)

data ObjectType
    = ObjectInt
    | ObjectLong
    | ObjectBoolean
    | ObjectString
    | ObjectPrimArray
    | ObjectArray
    | ObjectClass Ident
    deriving (Eq, Show)

data IncDec
    = SInc { _incDecMemory :: Memory, _incDecSize :: Size }
    | SDec { _incDecMemory :: Memory, _incDecSize :: Size }
    deriving (Eq, Show)

pattern Inc :: Memory -> Size -> InstrPayload
pattern Inc arg size = IIncDec (SInc arg size)

pattern Dec :: Memory -> Size -> InstrPayload
pattern Dec arg size = IIncDec (SDec arg size)

makeClassy ''Name
makeLenses ''Load
makeLenses ''Store
makeLenses ''BinOp
makeLenses ''UnOp
makeLenses ''GetAddr
makeLenses ''Call
makePrisms ''InstrPayload
makeLenses ''Operand
makePrisms ''OperandPayload
makePrisms ''Memory
makeLenses ''Instruction
makeLenses ''Block
makeLenses ''IncDec
makeLenses ''Object
makeLenses ''ObjectField
makeLenses ''PhiNode
makeLenses ''PhiBranch

instance Ord Block where
    compare = compare `on` _blockName

instance Show Block where
    show = views blockName show

class HasNames a where
    names :: Traversal' a Name

    default names :: HasName a => Lens' a Name
    names = name

instance HasNames Name

instance HasName Block where
    name = blockName

instance HasNames Block

instance HasName PhiNode where
    name = phiName

instance HasNames PhiNode

instance HasNames BlockEnd where
    names _ = pure

instance HasNames Instruction where
    names = instrResult . traverse

instance Pretty Ident where
    pPrint (Ident ident) = text (BS.unpack ident)

instance Pretty UniqueId where
    pPrint (UniqueId ident) = int ident

instance Pretty Operand where
    pPrint op = pPrint (op ^. operandSize) <+> "value" <+> pPrint (op ^. operandPayload)

instance Pretty OperandPayload where
    pPrint (OperandNamed n) = pPrint n
    pPrint (OperandInt i) = int i
    pPrint (OperandSize sz) = "sizeof" <+> pPrint sz
    pPrint OperandUndef = "undef"

instance Pretty Name where
    pPrint (Name i Nothing) = char '%' <> pPrint i
    pPrint (Name i (Just n)) = char '%' <> pPrint n <> char '.' <> pPrint i

instance Pretty Memory where
    pPrint (MemoryLocal i) = "local" <+> int i
    pPrint (MemoryArgument i) = "argument" <+> int i
    pPrint (MemoryOffset mem i sz disp) = pPrint mem <+> "+" <+> pPrint i <+> "*" <+> pPrint sz <+> "+" <+> int disp
    pPrint (MemoryGlobal i) = "global" <+> pPrint i
    pPrint MemoryUndef = "undef"

instance Pretty Size where
    pPrint Size8 = "8 bits"
    pPrint Size32 = "32 bits"
    pPrint Size64 = "64 bits"
    pPrint Size0 = "0 bits"
    pPrint SizePtr = "word"

instance Pretty Load where
    pPrint load = hsep
        [ "load"
        , parens $ pPrint (load ^. loadSize)
        , pPrint (load ^. loadFrom)
        ]

instance Pretty Store where
    pPrint store = hsep
        [ "store"
        , parens $ pPrint (store ^. storeSize)
        , pPrint (store ^. storeValue)
        , "into"
        , pPrint (store ^. storeTo)
        ]

instance Pretty BinOp where
    pPrint binOp = hsep
        [ pPrint (binOp ^. binOpLhs)
        , pPrint (binOp ^. binOpOp)
        , pPrint (binOp ^. binOpRhs)
        ]

instance Pretty BinOperator where
    pPrint BinOpPlus = "+"
    pPrint BinOpMinus = "-"
    pPrint BinOpTimes = "*"
    pPrint BinOpDivide = "/"
    pPrint BinOpModulo = "%"
    pPrint BinOpLess = "<"
    pPrint BinOpLessEqual = "<="
    pPrint BinOpGreater = ">"
    pPrint BinOpGreaterEqual = ">="
    pPrint BinOpEqual = "=="
    pPrint BinOpNotEqual = "="
    pPrint BinOpAnd = "&&"
    pPrint BinOpOr = "||"
    pPrint BinOpShiftLeft = "<<"
    pPrint BinOpShiftRight = ">>"

instance Pretty UnOp where
    pPrint unOp = hsep
        [ pPrint (unOp ^. unOpOp)
        , pPrint (unOp ^. unOpArg)
        ]

instance Pretty UnOperator where
    pPrint UnOpNeg = "-"
    pPrint UnOpNot = ""

instance Pretty GetAddr where
    pPrint getAddr = hsep
        [ "getAddr"
        , pPrint (getAddr ^. getAddrMem)
        ]

instance Pretty Call where
    pPrint call = "call" <+> pPrint (call ^. callDest) <> parens (hsep . punctuate comma $ map pPrint (call ^. callArgs))

instance Pretty Intristic where
    pPrint (IntristicAlloc size ty) = "alloc" <+> pPrint size <+> "bytes of" <+> pPrint ty
    pPrint (IntristicClone mem bytes) = "clone" <+> int bytes <+> "fields of" <+> pPrint mem
    pPrint (IntristicConcat lhs rhs) = "concat" <+> pPrint lhs <> comma <+> pPrint rhs

instance Pretty ObjectType where
    pPrint ObjectInt = "int"
    pPrint ObjectLong = "long"
    pPrint ObjectBoolean = "boolean"
    pPrint ObjectString = "string"
    pPrint ObjectPrimArray = "primArray"
    pPrint ObjectArray = "array"
    pPrint (ObjectClass cls) = "class" <+> pPrint cls

instance Pretty InstrPayload where
    pPrint (ILoad load) = pPrint load
    pPrint (IStore store) = pPrint store
    pPrint (IBinOp binOp) = pPrint binOp
    pPrint (IUnOp unOp) = pPrint unOp
    pPrint (IGetAddr getAddr) = pPrint getAddr
    pPrint (ICall call) = pPrint call
    pPrint (IIntristic intristic) = pPrint intristic
    pPrint (IIncDec incdec) = pPrint incdec
    pPrint (IConst op) = pPrint op

instance Pretty Instruction where
    pPrint instr = hsep
        [ maybe empty (\name -> pPrint name <+> "=") (instr ^. instrResult)
        , pPrint (instr ^. instrPayload)
        , "#"
        , hsep . punctuate semi $ map pPrint (instr ^. instrMetadata)
        ]

instance Pretty InstrMetadata where
    pPrint (InstrComment comment) = comment
    pPrint InstrInvariant = "invariant"
    pPrint (InstrLocation loc) = "at" <+> pPrint loc

instance Pretty Block where
    pPrint block = "block" <+> pPrint (block ^. blockName)

instance Pretty BlockEnd where
    pPrint (BlockEndBranch target) = "branch to" <+> pPrint target
    pPrint (BlockEndBranchCond cond targetTrue targetFalse) = hsep
        [ "branch to: if"
        ,  pPrint cond
        , "then"
        , pPrint targetTrue
        , "else"
        , pPrint targetFalse
        ]
    pPrint (BlockEndReturn ret) = "return" <+> pPrint ret
    pPrint BlockEndReturnVoid = "return"
    pPrint BlockEndNone = "must be unreachable"

instance Pretty IncDec where
    pPrint (SInc mem sz) = hsep
        [ "increment"
        , pPrint sz
        , "at"
        , pPrint mem]
    pPrint (SDec mem sz) = hsep
        [ "decrement"
        , pPrint sz
        , "at"
        , pPrint mem]

instance Pretty PhiNode where
    pPrint (PhiNode name branches) = pPrint name <+> "= phi" <+> sep (punctuate comma $ map pPrint branches)

instance Pretty PhiBranch where
    pPrint (PhiBranch from value) = pPrint value <+> "if from" <+> pPrint from

instance Pretty Object where
    pPrint (Object fields) = vcat (map pPrint fields)

instance Pretty ObjectField where
    pPrint field = pPrint $ field ^. objectFieldData

instance Pretty ObjectFieldData where
    pPrint (ObjectFieldInt i) = int i
    pPrint ObjectFieldNull = "null"
    pPrint (ObjectFieldRef ref) = "ref to" <+> pPrint ref

class PrettyIO a where
    pPrintIO :: MonadIO m => a -> m Doc

instance PrettyIO Block where
    pPrintIO blk = do
        phi <- fmap (vcat . map pPrint . toList) . liftIO . readIORef $ blk ^. blockPhi
        body <- fmap (vcat . map pPrint . toList) . liftIO . readIORef $ blk ^. blockBody
        end <- fmap pPrint . liftIO . readIORef $ blk ^. blockEnd
        pure $ pPrint (blk ^. name) $+$ nest 4 (phi $+$ body $+$ end)

successors :: MonadIO m => Block -> m [Block]
successors block = liftIO $ readIORef (block ^. blockEnd) >>= \case
    BlockEndNone -> pure []
    BlockEndBranch target -> pure [target]
    BlockEndBranchCond _ ifTrue ifFalse -> pure [ifFalse, ifTrue]
    BlockEndReturn _ -> pure []
    BlockEndReturnVoid -> pure []

-- |Returns the reachable blocks in reverse postorder
reachableBlocks :: MonadIO m => Block -> m [Block]
reachableBlocks block = snd <$> execStateT (go block) (Set.empty, [])
  where
    go :: (MonadIO m, MonadState (Set.Set Block, [Block]) m) => Block -> m ()
    go block = do
        alreadyVisited <- use $ _1 . contains block
        unless alreadyVisited $ do
            _1 . contains block .= True
            successors block >>= mapM_ go
            _2 %= cons block

predecessors :: MonadIO m => [Block] -> m (Map.Map Block [Block])
predecessors = foldlM go Map.empty
  where
    go acc block = foldl (add block) acc <$> successors block
    add block acc successor = acc & at successor . non [] %~ cons block

nameToIdent :: Name -> Ident
nameToIdent name = Ident . BS.pack $ '.' : show (views nameUnique getUniqueId name)

class HasOperands a where
    operands :: Traversal' a Operand 

instance HasOperands Operand where
    operands = id

instance HasOperands Memory where
    operands f (MemoryOffset base idx sz disp) = MemoryOffset <$> operands f base <*> operands f idx <*> pure sz <*> pure disp
    operands _ o = pure o

instance HasOperands BlockEnd where
    operands f (BlockEndBranchCond cond true false) = BlockEndBranchCond <$> operands f cond <*> pure true <*> pure false
    operands f (BlockEndReturn ret) = BlockEndReturn <$> operands f ret
    operands _ o = pure o

instance HasOperands PhiBranch where
    operands f (PhiBranch from value) = PhiBranch <$> pure from <*> operands f value

instance HasOperands PhiNode where
    operands f (PhiNode name branches) = PhiNode <$> pure name <*> operands f branches

instance HasOperands a => HasOperands [a] where
    operands = traverse . operands

instance HasOperands a => HasOperands (Seq.Seq a) where
    operands = traverse . operands

instance HasOperands Load where
    operands f (SLoad from size) = SLoad <$> operands f from <*> pure size

instance HasOperands Store where
    operands f (SStore to size value) = SStore <$> operands f to <*> pure size <*> operands f value

instance HasOperands BinOp where
    operands f (SBinOp lhs op rhs) = SBinOp <$> operands f lhs <*> pure op <*> operands f rhs

instance HasOperands UnOp where
    operands f (SUnOp op arg) = SUnOp <$> pure op <*> operands f arg

instance HasOperands GetAddr where
    operands f (SGetAddr mem) = SGetAddr <$> operands f mem

instance HasOperands Call where
    operands f (SCall dest args) = SCall <$> operands f dest <*> operands f args

instance HasOperands Intristic where
    operands f (IntristicAlloc op ot) = IntristicAlloc <$> operands f op <*> pure ot
    operands f (IntristicClone mem size) = IntristicClone <$> operands f mem <*> pure size
    operands f (IntristicConcat lhs rhs) = IntristicConcat <$> operands f lhs <*> operands f rhs

instance HasOperands IncDec where
    operands f (SInc mem size) = SInc <$> operands f mem <*> pure size
    operands f (SDec mem size) = SDec <$> operands f mem <*> pure size

instance HasOperands InstrPayload where
    operands f (ILoad load) = ILoad <$> operands f load
    operands f (IStore store) = IStore <$> operands f store
    operands f (IBinOp binOp) = IBinOp <$> operands f binOp
    operands f (IUnOp unOp) = IUnOp <$> operands f unOp
    operands f (ICall call) = ICall <$> operands f call
    operands f (IIntristic intristic) = IIntristic <$> operands f intristic
    operands f (IIncDec incdec) = IIncDec <$> operands f incdec
    operands f (IConst const) = IConst <$> operands f const
    operands f (IGetAddr getaddr) = IGetAddr <$> operands f getaddr

instance HasOperands Instruction where
    operands f (Instruction result payload meta) = Instruction <$> pure result <*> operands f payload <*> pure meta
