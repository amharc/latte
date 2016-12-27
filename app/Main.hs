{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Language.Latte.Frontend.AST as P
import qualified Language.Latte.Frontend.Parser as P
import Text.Parsec.ByteString
import System.Environment
import System.IO

main :: IO ()
main = getArgs >>= \case
    [file] -> parseFromFile P.program file >>= either (hPrint stderr) (putStr . P.pretty)
    _ -> fail "You cannot into arguments"
