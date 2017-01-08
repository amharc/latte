{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE StrictData #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Latte.Backend.Asm where

import Control.Lens
import Data.Data
import qualified Data.Set as Set
import Data.ByteString.Char8 as BS

data Register = RSP | RBP | RBX | RCX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | RDX | RAX | RFLAGS
    deriving (Eq, Show, Ord, Enum, Bounded, Data, Typeable)

data RegisterOrSpill = RSRegister Register | RSSpill Int
    deriving (Eq, Show, Ord, Data, Typeable)

data Operand = OpRegister Register | OpMemory Memory | OpImmediate Int
    deriving (Eq, Show, Data, Typeable)

data Memory
    = Memory
        { _memBase :: Register
        , _memIndex :: Maybe (Register, Multiplier)
        , _memDisplacement :: {-# UNPACK #-} Int
        }
#if __GLASGOW_HASKELL__ >= 801
    | Global ~Ident
#else
    | Global Ident
#endif
    deriving (Eq, Show, Data, Typeable)

data Multiplier = Mult1 | Mult2 | Mult4 | Mult8
    deriving (Eq, Ord, Show, Data, Typeable)

data Flag = FlagEqual | FlagNotEqual | FlagLess | FlagLessEqual | FlagGreater | FlagGreaterEqual
    deriving (Eq, Show, Data, Typeable)

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
    | Sal Multiplier Operand Operand
    | Sar Multiplier Operand Operand
    | Idiv Multiplier Operand
    | Cmp Multiplier Operand Operand
    | Neg Multiplier Operand
    | Set Flag Operand
    | Movsbq Operand Operand
    | Xchg Multiplier Operand Operand
    | Test Multiplier Operand Operand
    | Cqto
    | Inc Multiplier Operand
    | Dec Multiplier Operand
    | Jump Operand
#if __GLASGOW_HASKELL__ >= 801
    | JumpCond Flag ~Ident
#else
    | JumpCond Flag Ident
#endif
    | Label Ident
    | GlobalFunc Ident
    | Section BS.ByteString
    | Type Ident BS.ByteString
    | String BS.ByteString
    | Push Multiplier Operand
    | Pop Multiplier Operand
    | Quad Ident
    | QuadInt Int
    | Align Int
    | Call Operand
    | AnnotateLiveStart (Set.Set Register)
    | AnnotateLiveStop
    | Leave
    | Ret
    deriving (Eq, Show, Data, Typeable)

makeLenses ''Memory
makePrisms ''RegisterOrSpill
makePrisms ''Operand

negFlag :: Flag -> Flag
negFlag FlagEqual = FlagNotEqual
negFlag FlagNotEqual = FlagEqual
negFlag FlagLess = FlagGreaterEqual
negFlag FlagLessEqual = FlagGreater
negFlag FlagGreater = FlagLessEqual
negFlag FlagGreaterEqual = FlagLess
