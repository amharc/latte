{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.IR where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.IORef
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
    { _objectFieldData :: !ObjectFieldData
    }

data ObjectFieldData
    = ObjectFieldInt !Int
    | ObjectFieldNull
    | ObjectFieldRef !Ident

data Operand
    = OperandNamed !Name
    | OperandSize !Size
    | OperandInt !Int
    | OperandUndef
    deriving Show

data Name = Name
    { _nameUnique :: {-# UNPACK #-} !UniqueId
    , _nameHuman :: !(Maybe Ident)
    }
    deriving (Eq, Ord, Show)

data Memory
    = MemoryLocal {-# UNPACK #-} !Int
    | MemoryArgument {-# UNPACK #-} !Int
    | MemoryOffset !Operand !Operand !Size
    | MemoryGlobal !Ident
    | MemoryUndef
    deriving Show

pattern MemoryPointer :: Operand -> Memory
pattern MemoryPointer ptr = MemoryOffset ptr (OperandInt 0) SizePtr

data Size = Size0 | Size8 | Size32 | Size64 | SizePtr
    deriving (Eq, Ord, Show)

data InstrPayload
    = ILoad !Load
    | IStore !Store
    | IBinOp !BinOp
    | IUnOp !UnOp
    | IGetAddr !GetAddr
    | ICall !Call
    | IIntristic !Intristic
    | IIncDec !IncDec
    | IConst !Operand
    deriving Show

data Instruction = Instruction
    { _instrResult :: !Name
    , _instrPayload :: !InstrPayload
    , _instrMetadata :: [InstrMetadata]
    }
    deriving Show

data InstrMetadata
    = InstrComment !Doc
    | InstrInvariant
    | InstrLocation !Frontend.LocRange
    deriving Show

data Block = Block
    { _blockName :: !Name
    , _blockPhi :: {-# UNPACK #-} !(IORef (Seq.Seq PhiNode))
    , _blockBody :: {-# UNPACK #-} !(IORef (Seq.Seq Instruction))
    , _blockEnd :: {-# UNPACK #-} !(IORef BlockEnd)
    }
    deriving Eq

data PhiNode = PhiNode
    { _phiName :: !Name
    , _phiBranches :: [PhiBranch]
    }

data PhiBranch = PhiBranch
    { _phiFrom :: !Block
    , _phiValue :: !Operand
    }

data BlockEnd
    = BlockEndBranch !Block
    | BlockEndBranchCond !Operand !Block !Block
    | BlockEndReturn !Operand
    | BlockEndReturnVoid
    | BlockEndNone

data Load = SLoad
    { _loadFrom :: !Memory
    , _loadSize :: !Size
    }
    deriving Show

pattern Load :: Memory -> Size -> InstrPayload
pattern Load from size = ILoad (SLoad from size)

data Store = SStore
    { _storeTo :: !Memory
    , _storeSize :: !Size
    , _storeValue :: !Operand
    }
    deriving Show

pattern Store :: Memory -> Size -> Operand -> InstrPayload
pattern Store to size value = IStore (SStore to size value)

data BinOp = SBinOp
    { _binOpLhs :: !Operand
    , _binOpOp ::  !BinOperator
    , _binOpRhs :: !Operand
    }
    deriving Show

pattern BinOp :: Operand -> BinOperator -> Operand -> InstrPayload
pattern BinOp lhs op rhs = IBinOp (SBinOp lhs op rhs)

data UnOp = SUnOp
    { _unOpOp :: !UnOperator
    , _unOpArg :: !Operand
    }
    deriving Show

pattern UnOp :: UnOperator -> Operand -> InstrPayload
pattern UnOp op arg = IUnOp (SUnOp op arg)

newtype GetAddr = SGetAddr { _getAddrMem :: Memory }
    deriving Show

pattern GetAddr :: Memory -> InstrPayload
pattern GetAddr mem = IGetAddr (SGetAddr mem)

data Call = SCall
    { _callDest :: !Memory
    , _callArgs :: [Operand]
    }
    deriving Show

pattern Call :: Memory -> [Operand] -> InstrPayload
pattern Call dest args = ICall (SCall dest args)

data BinOperator
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

data UnOperator
    = UnOpNeg
    | UnOpNot
    deriving Show

data Intristic
    = IntristicAlloc !Operand !ObjectType
    | IntristicClone !Memory
    | IntristicConcat !Operand !Operand
    deriving Show

data ObjectType
    = ObjectInt
    | ObjectLong
    | ObjectBoolean
    | ObjectString
    | ObjectPrimArray
    | ObjectArray
    | ObjectClass !Ident
    deriving Show

data IncDec
    = SInc { _incDecMemory :: !Memory, _incDecSize :: !Size }
    | SDec { _incDecMemory :: !Memory, _incDecSize :: !Size }
    deriving Show

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
makeLenses ''Instruction
makeLenses ''Block
makeLenses ''IncDec
makeLenses ''Object
makeLenses ''ObjectField

instance HasName Block where
    name = blockName

instance HasName Instruction where
    name = instrResult

instance Pretty Ident where
    pPrint (Ident ident) = text (BS.unpack ident)

instance Pretty UniqueId where
    pPrint (UniqueId ident) = int ident

instance Pretty Operand where
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
    pPrint (MemoryOffset mem i sz) = pPrint mem <+> "+" <+> pPrint i <+> "*" <+> pPrint sz
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
    pPrint BinOpNotEqual = "!="
    pPrint BinOpAnd = "&&"
    pPrint BinOpOr = "||"

instance Pretty UnOp where
    pPrint unOp = hsep
        [ pPrint (unOp ^. unOpOp)
        , pPrint (unOp ^. unOpArg)
        ]

instance Pretty UnOperator where
    pPrint UnOpNeg = "-"
    pPrint UnOpNot = "!"

instance Pretty GetAddr where
    pPrint getAddr = hsep
        [ "getAddr"
        , pPrint (getAddr ^. getAddrMem)
        ]

instance Pretty Call where
    pPrint call = "call" <+> pPrint (call ^. callDest) <> parens (sep . punctuate comma $ map pPrint (call ^. callArgs))

instance Pretty Intristic where
    pPrint (IntristicAlloc size ty) = "alloc" <+> pPrint size <+> "bytes of" <+> pPrint ty
    pPrint (IntristicClone mem) = "clone" <+> pPrint mem
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
        [ pPrint (instr ^. instrResult)
        , "="
        , pPrint (instr ^. instrPayload)
        , "#"
        , hsep . punctuate semi $ map pPrint (instr ^. instrMetadata)
        ]

instance Pretty InstrMetadata where
    pPrint (InstrComment comment) = comment
    pPrint InstrInvariant = "!invariant"
    pPrint (InstrLocation loc) = "at" <+> pPrint loc

instance Pretty Block where
    pPrint block = "block" <+> pPrint (block ^. blockName)

instance Pretty BlockEnd where
    pPrint (BlockEndBranch target) = "branch to" <+> pPrint target
    pPrint (BlockEndBranchCond cond targetTrue targetFalse) = sep
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
    pPrint (SInc mem sz) = sep
        [ "increment"
        , pPrint sz
        , "at"
        , pPrint mem]
    pPrint (SDec mem sz) = sep
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
    BlockEndBranchCond _ ifTrue ifFalse -> pure [ifTrue, ifFalse]
    BlockEndReturn _ -> pure []
    BlockEndReturnVoid -> pure []

reachableBlocks :: MonadIO m => Block -> m [Block]
reachableBlocks start = go (Seq.singleton start) Set.empty
  where
    go :: MonadIO m => Seq.Seq Block -> Set.Set Name -> m [Block]
    go queue visited = case Seq.viewl queue of
        Seq.EmptyL -> pure []
        block Seq.:< blocks -> do
            succs <- successors block
            let (queue, set) = foldl add (blocks, visited) succs
            (block:) <$> go queue set

    add (queue, set) block
        | Set.member (block ^. blockName) set = (queue, set)
        | otherwise = (queue |> block, Set.insert (block ^. blockName) set)


nameToIdent :: Name -> Ident
nameToIdent name = Ident . BS.pack $ '.' : show (views nameUnique getUniqueId name)
