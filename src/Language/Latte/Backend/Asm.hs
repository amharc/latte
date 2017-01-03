{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Latte.Backend.Asm where

import Control.Lens
import Data.Data
import Data.ByteString.Char8 as BS

data Register = RSP | RBP | RBX | RCX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | RDX | RAX
    deriving (Eq, Show, Ord, Enum, Bounded, Data, Typeable)

data RegisterOrSpill = RSRegister Register | RSSpill Int
    deriving (Eq, Show, Ord, Data, Typeable)

data Operand = OpRegister Register | OpMemory Memory | OpImmediate Int
    deriving (Eq, Data, Typeable)

data Memory
    = Memory
        { _memBase :: Register
        , _memIndex :: Maybe (Register, Multiplier)
        , _memDisplacement :: {-# UNPACK #-} Int
        }
    | Global Ident
    deriving (Eq, Data, Typeable)

data Multiplier = Mult1 | Mult2 | Mult4 | Mult8
    deriving (Eq, Ord, Show, Data, Typeable)

data Flag = FlagEqual | FlagNotEqual | FlagLess | FlagLessEqual | FlagGreater | FlagGreaterEqual

newtype Ident = Ident { getIdent :: BS.ByteString }
    deriving (Eq, Ord, Show, Data, Typeable)

data Instruction
    = Add Multiplier Operand Operand
    | Sub Multiplier Operand Operand
    | Imul Multiplier Operand Operand
    | Lea Multiplier Operand Operand
    | Mov Multiplier Operand Operand
    | And Multiplier Operand Operand
    | Or Multiplier Operand Operand
    | Xor Multiplier Operand Operand
    | Idiv Multiplier Operand
    | Cmp Multiplier Operand Operand
    | Neg Multiplier Operand
    | Set Flag Operand
    | Movsbq Operand Operand
    | Test Operand Operand
    | Cltq
    | Inc Multiplier Operand
    | Dec Multiplier Operand
    | Jump Ident
    | JumpCond Flag Ident
    | Label Ident
    | GlobalFunc Ident
    | Call Operand
    | Leave
    | Ret

makeLenses ''Memory
makePrisms ''RegisterOrSpill
makePrisms ''Operand
