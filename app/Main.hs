{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import qualified Language.Latte.Frontend.AST as P
import qualified Language.Latte.Frontend.Parser as P
import qualified Language.Latte.Frontend.GenIR as P
import qualified Language.Latte.Middleend.Monad as M
import qualified Language.Latte.Middleend.MemToReg as MemToReg
import qualified Language.Latte.Middleend.SimplifyPhi as SimplifyPhi
import qualified Language.Latte.Middleend.SimplifyControlFlow as SimplifyControlFlow
import qualified Language.Latte.Middleend.ShrinkEnds as ShrinkEnds
import qualified Language.Latte.Middleend.Propagate as Propagate
import qualified Language.Latte.Middleend.DeadCodeElimination as DeadCodeElimination
import qualified Language.Latte.Middleend.StrengthReduction as StrengthReduction
import qualified Language.Latte.Middleend.Fixed as Fixed
import qualified Language.Latte.Backend.CodeGen as B
import qualified Language.Latte.Backend.Stringify as B
import Text.Parsec.ByteString
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import System.Environment
import System.IO

middle :: P.Program -> IO ()
middle program = do
    hPutStrLn stderr "Parsed:"
    hPutStrLn stderr . render $ pPrint program

    diags <- M.run $ do
        P.generateIR program
        M.debugState >>= liftIO . hPutStrLn stderr . render 

        liftIO $ hPutStrLn stderr "\n\nMemToReg\n\n"
        MemToReg.opt
        M.debugState >>= liftIO . hPutStrLn stderr . render 
        Fixed.iterOpt 1000 $ do
            liftIO $ hPutStr stderr "\n\nSimplifyPhi\n\n"
            SimplifyPhi.opt
            M.debugState >>= liftIO . hPutStrLn stderr . render 

            liftIO $ hPutStr stderr "\n\nShrinkEnds\n\n"
            ShrinkEnds.opt
            M.debugState >>= liftIO . hPutStrLn stderr . render 

            liftIO $ hPutStr stderr "\n\nStrength reduction\n\n"
            StrengthReduction.opt
            M.debugState >>= liftIO . hPutStrLn stderr . render

            liftIO $ hPutStr stderr "\n\nPropagate\n\n"
            Propagate.opt
            M.debugState >>= liftIO . hPutStrLn stderr . render 

            liftIO $ hPutStr stderr "\n\nSimplify control flow\n\n"
            SimplifyControlFlow.opt
            M.debugState >>= liftIO . hPutStrLn stderr . render 

            liftIO $ hPutStr stderr "\n\nRemove dead code\n\n"
            DeadCodeElimination.opt
            M.debugState >>= liftIO . hPutStrLn stderr . render

        get >>= B.emitState >>= liftIO . flip B.translateOut stdout

    forM_ diags $ \diag ->
        hPutStrLn stderr . render $ pPrint diag


main :: IO ()
main = getArgs >>= \case
    [file] -> parseFromFile P.program file >>= either (hPrint stderr) middle
    _ -> fail "You cannot into arguments"
