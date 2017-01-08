{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Control.Monad.State
import qualified Language.Latte.Frontend.AST as P
import qualified Language.Latte.Frontend.Parser as P
import qualified Language.Latte.Frontend.GenIR as P
import qualified Language.Latte.Middleend.Monad as M
import qualified Language.Latte.Middleend.CheckUnreachability as CheckUnreachability
import qualified Language.Latte.Middleend.MemToReg as MemToReg
import qualified Language.Latte.Middleend.SimplifyPhi as SimplifyPhi
import qualified Language.Latte.Middleend.SimplifyControlFlow as SimplifyControlFlow
import qualified Language.Latte.Middleend.ShrinkEnds as ShrinkEnds
import qualified Language.Latte.Middleend.Propagate as Propagate
import qualified Language.Latte.Middleend.DeadCodeElimination as DeadCodeElimination
import qualified Language.Latte.Middleend.StrengthReduction as StrengthReduction
import qualified Language.Latte.Middleend.TailCalls as TailCalls
import qualified Language.Latte.Middleend.Fixed as Fixed
import qualified Language.Latte.Backend.CodeGen as B
import qualified Language.Latte.Backend.Stringify as B
import qualified Language.Latte.Backend.Peephole as Peephole
import Text.Parsec.ByteString
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import System.Environment
import System.Exit
import System.FilePath
import System.IO

middle :: FilePath -> P.Program -> IO ()
middle file program = do
    diags <- M.run $ do
        P.generateIR program

        MemToReg.opt
        Fixed.iterOpt 1000 $ do
            SimplifyPhi.opt
            ShrinkEnds.opt
            StrengthReduction.opt
            Propagate.opt
            SimplifyControlFlow.opt
            DeadCodeElimination.opt

        TailCalls.opt
        CheckUnreachability.check

        M.whenNoDiagnostics $ do
            liftIO $ hPutStrLn stderr "OK"
            asm <- get >>= B.emitState
            let asmFile = file -<.> "s"
            liftIO $ withFile asmFile WriteMode $ B.translateOut (Peephole.opt asm)
            liftIO $ exitSuccess

    hPutStrLn stderr "ERROR"
    forM_ diags $ \diag ->
        hPutStrLn stderr . render $ pPrint diag
    exitFailure

main :: IO ()
main = getArgs >>= \case
    [file] -> parseFromFile P.program file >>= \case
        Right ast -> middle file ast
        Left err -> do
            hPutStrLn stderr "ERROR"
            hPrint stderr err
            exitFailure
    _ -> fail "You cannot into arguments"
