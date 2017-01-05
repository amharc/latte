{-# LANGUAGE OverloadedStrings #-}
module Language.Latte.Backend.Stringify (translateOut) where

import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSShort
import qualified Data.ByteString.Builder as BS
import qualified Data.Sequence as Seq
import Language.Latte.Backend.Asm
import GHC.IO.Handle

translate :: Foldable f => f Instruction -> BS.Builder
translate = foldMap (\x -> trans x <> sbs "\n")

translateOut :: Foldable f => f Instruction -> Handle -> IO ()
{-# SPECIALIZE translateOut :: Seq.Seq Instruction -> Handle -> IO () #-}
translateOut instrs handle = BS.hPutBuilder handle (translate instrs)

class Trans a where
    trans :: a -> BS.Builder
    trans = transMult Mult8

    transMult :: Multiplier -> a -> BS.Builder
    transMult _ = trans

instance Trans Register where
    transMult Mult8 RAX = sbs "%rax"
    transMult Mult8 RBX = sbs "%rbx"
    transMult Mult8 RCX = sbs "%rcx"
    transMult Mult8 RDX = sbs "%rdx"
    transMult Mult8 RSI = sbs "%rsi"
    transMult Mult8 RDI = sbs "%rdi"
    transMult Mult8 R8 = sbs "%r8"
    transMult Mult8 R9 = sbs "%r9"
    transMult Mult8 R10 = sbs "%r10"
    transMult Mult8 R11 = sbs "%r11"
    transMult Mult8 R12 = sbs "%r12"
    transMult Mult8 R13 = sbs "%r13"
    transMult Mult8 R14 = sbs "%r14"
    transMult Mult8 R15 = sbs "%r15"
    transMult Mult8 RSP = sbs "%rsp"
    transMult Mult8 RBP = sbs "%rbp"

    transMult Mult4 RAX = sbs "%eax"
    transMult Mult4 RBX = sbs "%ebx"
    transMult Mult4 RCX = sbs "%ecx"
    transMult Mult4 RDX = sbs "%edx"
    transMult Mult4 RSI = sbs "%esi"
    transMult Mult4 RDI = sbs "%edi"
    transMult Mult4 R8 = sbs "%r8d"
    transMult Mult4 R9 = sbs "%r9d"
    transMult Mult4 R10 = sbs "%r10d"
    transMult Mult4 R11 = sbs "%r11d"
    transMult Mult4 R12 = sbs "%r12d"
    transMult Mult4 R13 = sbs "%r13d"
    transMult Mult4 R14 = sbs "%r14d"
    transMult Mult4 R15 = sbs "%r15d"
    transMult Mult4 RSP = sbs "%esp"
    transMult Mult4 RBP = sbs "%epp"

    transMult Mult2 RAX = sbs "%ax"
    transMult Mult2 RBX = sbs "%bx"
    transMult Mult2 RCX = sbs "%cx"
    transMult Mult2 RDX = sbs "%dx"
    transMult Mult2 RSI = sbs "%si"
    transMult Mult2 RDI = sbs "%di"
    transMult Mult2 R8 = sbs "%r8w"
    transMult Mult2 R9 = sbs "%r9w"
    transMult Mult2 R10 = sbs "%r10w"
    transMult Mult2 R11 = sbs "%r11w"
    transMult Mult2 R12 = sbs "%r12w"
    transMult Mult2 R13 = sbs "%r13w"
    transMult Mult2 R14 = sbs "%r14w"
    transMult Mult2 R15 = sbs "%r15w"
    transMult Mult2 RSP = sbs "%sp"
    transMult Mult2 RBP = sbs "%bp"

    transMult Mult1 RAX = sbs "%al"
    transMult Mult1 RBX = sbs "%bl"
    transMult Mult1 RCX = sbs "%cl"
    transMult Mult1 RDX = sbs "%dl"
    transMult Mult1 RSI = sbs "%sil"
    transMult Mult1 RDI = sbs "%dil"
    transMult Mult1 R8 = sbs "%r8b"
    transMult Mult1 R9 = sbs "%r9b"
    transMult Mult1 R10 = sbs "%r10b"
    transMult Mult1 R11 = sbs "%r11b"
    transMult Mult1 R12 = sbs "%r12b"
    transMult Mult1 R13 = sbs "%r13b"
    transMult Mult1 R14 = sbs "%r14b"
    transMult Mult1 R15 = sbs "%r15b"
    transMult Mult1 RSP = sbs "%spl"
    transMult Mult1 RBP = sbs "%bpl"

instance Trans Operand where
    transMult mult (OpRegister reg) = transMult mult reg
    transMult mult (OpMemory mem) = transMult mult mem
    transMult _ (OpImmediate int) = sbs "$" <> BS.intDec int

instance Trans Memory where
    trans (Memory base (Just (reg, mult)) disp) = mconcat
        [ BS.intDec disp
        , sbs "("
        , trans base
        , sbs ","
        , trans reg
        , sbs ","
        , case mult of
            Mult1 -> sbs "1"
            Mult2 -> sbs "2"
            Mult4 -> sbs "4"
            Mult8 -> sbs "8"
        , sbs ")"
        ]
    trans (Memory base Nothing disp) = mconcat
        [ BS.intDec disp
        , sbs "("
        , trans base
        , sbs ")"
        ]
    trans (Global ident) = trans ident

instance Trans Ident where
    trans = BS.byteString . getIdent

instance Trans Flag where
    trans FlagEqual = sbs "e"
    trans FlagNotEqual = sbs "ne"
    trans FlagLess = sbs "l"
    trans FlagLessEqual = sbs "le"
    trans FlagGreater = sbs "g"
    trans FlagGreaterEqual = sbs "ge"

instance Trans Instruction where
    trans (Add mult lhs rhs) = basicInstr "add" mult lhs rhs
    trans (Sub mult lhs rhs) = basicInstr "sub" mult lhs rhs
    trans (Imul mult lhs rhs) = basicInstr "imul" mult lhs rhs
    trans (Lea mult lhs rhs) = basicInstr "lea" mult lhs rhs
    trans (Mov mult lhs rhs) = basicInstr "mov" mult lhs rhs
    trans (And mult lhs rhs) = basicInstr "and" mult lhs rhs
    trans (Or mult lhs rhs) = basicInstr "or" mult lhs rhs
    trans (Xor mult lhs rhs) = basicInstr "xor" mult lhs rhs
    trans (Sal mult lhs rhs) = basicInstr "sal" mult lhs rhs
    trans (Sar mult lhs rhs) = basicInstr "sar" mult lhs rhs
    trans (Idiv mult op) = mconcat
        [ sbs "\tidiv"
        , suffix mult
        , sbs " "
        , transMult mult op
        ]
    trans (Cmp mult lhs rhs) = basicInstr "cmp" mult lhs rhs
    trans (Neg mult op) = sbs "\tneg" <> suffix mult <> sbs " " <> transMult mult op
    trans (Set flag op) = sbs "\tset" <> trans flag <> sbs " " <> transMult Mult1 op
    trans (Movsbq lhs rhs) = sbs "\tmovsbq " <> transMult Mult1 lhs <> ", " <> transMult Mult8 rhs
    trans (Test mult lhs rhs) = sbs "\ttest" <> suffix mult <> " " <> transMult mult lhs <> ", " <> transMult mult rhs
    trans Cqto = sbs "\tcqto"
    trans (Inc _ op) = sbs "\tinc " <> trans op
    trans (Dec _ op) = sbs "\tdec " <> trans op
    trans (JumpCond flag ident) = sbs "\tj" <> trans flag <> sbs " " <> trans ident
    trans (Jump ident) = sbs "\tjmp " <> trans ident
    trans (GlobalFunc ident) = sbs ".global " <> trans ident
    trans (Call op) = sbs "\tcall " <> trans op
    trans Leave = sbs "\tleave"
    trans Ret = sbs "\tret"
    trans (Label ident) = trans ident <> sbs ":"
    trans (Section sect) = sbs ".section " <> BS.byteString sect
    trans (Type ident ty) = sbs ".type " <> trans ident <> ", " <> BS.byteString ty
    trans (Quad ident) = sbs "\t.quad " <> trans ident
    trans (QuadInt i) = sbs "\t.quad " <> BS.intDec i
    trans (Align i) = sbs ".align " <> BS.intDec i
    trans (String str) = mconcat
        [ sbs ".string \""
        , BS.byteString $ BS.concatMap escape str
        , sbs "\""
        ]
      where
        escape '\\' = "\\\\"
        escape '\"' = "\\\""
        escape c = BS.singleton c
    trans (Push mult op) = sbs "\tpush" <> suffix mult <> " " <> transMult mult op
    trans (Pop mult op) = sbs "\tpop" <> suffix mult <> " " <> transMult mult op
    trans (AnnotateLiveStart regs) = sbs "#live registers analysis start: " <> foldMap (\reg -> trans reg <> sbs " ") regs
    trans AnnotateLiveStop = sbs "#live registers analysis stop"

sbs :: BSShort.ShortByteString -> BS.Builder
sbs = BS.shortByteString

suffix :: Multiplier -> BS.Builder
suffix Mult1 = sbs "b"
suffix Mult2 = sbs "w"
suffix Mult4 = sbs "l"
suffix Mult8 = sbs "q"

basicInstr :: (Trans a, Trans b) => BSShort.ShortByteString -> Multiplier -> a -> b -> BS.Builder
basicInstr pref mult lhs rhs = mconcat
    [ sbs "\t"
    , sbs pref
    , suffix mult
    , sbs " "
    , transMult mult lhs
    , sbs ", "
    , transMult mult rhs
    ]
