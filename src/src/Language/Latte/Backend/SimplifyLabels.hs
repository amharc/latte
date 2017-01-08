{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Backend.Peephole(opt) where

import Control.Lens
import Data.Data
import Data.Data.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Language.Latte.Backend.Asm

opt :: Seq.Seq Instruction -> Seq.Seq Instruction
opt code = Seq.foldr (applySubst (Seq.foldr computeSubst Map.empty code)) Seq.empty code

type Subst = Map.Map Ident Ident

computeSubst :: 
