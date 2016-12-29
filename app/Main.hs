{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Language.Latte.Frontend.AST as P
import qualified Language.Latte.Frontend.Parser as P
import qualified Language.Latte.Middleend.GenIR as M
import qualified Language.Latte.Middleend.Monad as M
import qualified Language.Latte.Middleend.MemToReg as MemToReg
import qualified Language.Latte.Middleend.SimplifyPhi as SimplifyPhi
import qualified Language.Latte.Middleend.SimplifyControlFlow as SimplifyControlFlow
import qualified Language.Latte.Middleend.ShrinkEnds as ShrinkEnds
import qualified Language.Latte.Middleend.Propagate as Propagate
import qualified Language.Latte.Middleend.DeadCodeElimination as DeadCodeElimination
import qualified Language.Latte.Middleend.Fixed as Fixed
import Text.Parsec.ByteString
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import System.Environment
import System.IO

middle :: P.Program -> IO ()
middle program = do
    putStrLn "Parsed:"
    putStrLn . render $ pPrint program

    diags <- M.run $ do
        M.generateIR program
        M.debugState >>= liftIO . putStrLn . render 

        liftIO $ putStrLn "\n\nMemToReg\n\n"
        MemToReg.opt
        M.debugState >>= liftIO . putStrLn . render 

        Fixed.iterOpt 1000 $ do
            liftIO $ putStr "\n\nSimplifyPhi\n\n"
            SimplifyPhi.opt
            M.debugState >>= liftIO . putStrLn . render 

            liftIO $ putStr "\n\nShrinkEnds\n\n"
            ShrinkEnds.opt
            M.debugState >>= liftIO . putStrLn . render 

            liftIO $ putStr "\n\nPropagate\n\n"
            Propagate.opt
            M.debugState >>= liftIO . putStrLn . render 

            liftIO $ putStr "\n\nSimplify control flow\n\n"
            SimplifyControlFlow.opt
            M.debugState >>= liftIO . putStrLn . render 

            liftIO $ putStr "\n\nRemove dead code\n\n"
            DeadCodeElimination.opt
            M.debugState >>= liftIO . putStrLn . render 

    forM_ diags $ \diag ->
        hPutStrLn stderr . render $ pPrint diag

main :: IO ()
main = getArgs >>= \case
    [file] -> parseFromFile P.program file >>= either (hPrint stderr) middle
    _ -> fail "You cannot into arguments"
