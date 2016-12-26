{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Latte.Frontend.Parser where

import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Either (partitionEithers)
import Data.Semigroup ((<>))
import Language.Latte.Frontend.AST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.ByteString
import qualified Text.Parsec.Token as P

latteDef :: P.GenLanguageDef BS.ByteString u Identity
latteDef = P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = False
    , P.identStart = letter
    , P.identLetter = alphaNum <|> oneOf "_'"
    , P.reservedNames = ["int", "void", "boolean", "string", "return", "for", "while", "new", "class", "extends", "null"]
    , P.reservedOpNames = ["+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "!", "||", "&&", "="]
    , P.opStart = P.opLetter latteDef
    , P.opLetter = oneOf "+-*/%<>=!&|"
    , P.caseSensitive = True
    }

identifier :: Parser String
reserved :: String -> Parser ()
reservedOp :: String -> Parser ()
operator :: Parser String
charLiteral :: Parser Char
stringLiteral :: Parser String
natural :: Parser Integer
integer :: Parser Integer
float :: Parser Double
naturalOrFloat :: Parser (Either Integer Double)
decimal :: Parser Integer
hexadecimal :: Parser Integer
octal :: Parser Integer
symbol :: String -> Parser String
lexeme :: Parser a -> Parser a
whiteSpace :: Parser ()
parens :: Parser a -> Parser a
braces :: Parser a -> Parser a
angles :: Parser a -> Parser a
brackets :: Parser a -> Parser a
squares :: Parser a -> Parser a
semi :: Parser String
comma :: Parser String
colon :: Parser String
dot :: Parser String
semiSep :: Parser a -> Parser [a]
semiSep1 :: Parser a -> Parser [a]
commaSep :: Parser a -> Parser [a]
commaSep1 :: Parser a -> Parser [a]
P.TokenParser{..} = P.makeTokenParser latteDef

opsTable :: [[Operator BS.ByteString () Identity (Located Expr)]]
opsTable =
    [ [ unary "!" UnOpNot
      , unary "-" UnOpNeg
      ]
    , [ binary "*" BinOpTimes AssocLeft
      , binary "/" BinOpDivide AssocLeft
      , binary "%" BinOpModulo AssocLeft
      ]
    , [ binary "+" BinOpPlus AssocLeft
      , binary "-" BinOpMinus AssocLeft
      ]
    , [ binary "<" BinOpLess AssocNone
      , binary "<=" BinOpLessEqual AssocNone
      , binary ">" BinOpGreater AssocNone
      , binary ">=" BinOpGreaterEqual AssocNone
      , binary "==" BinOpEqual AssocNone
      , binary "!=" BinOpNotEqual AssocNone
      ]
    , [ binary "&&" BinOpAnd AssocLeft
      , binary "||" BinOpOr AssocLeft
      ]
    ]
  where
    binary oper ctor = Infix $ do
        reservedOp oper
        pure . meld $ \lhs rhs -> ExprBinOp lhs ctor rhs
    unary oper ctor = Prefix $ do
        o <- located (reservedOp oper)
        pure $ meld (const $ ExprUnOp ctor) o
    meld f lhs rhs = Located (lhs ^. loc <> rhs ^. loc) $ f lhs rhs

ident :: Parser Ident
ident = Ident . BS.pack <$> identifier

location :: Parser Location
location = do
    sourcePos <- getPosition
    pure $ Location (sourceLine sourcePos) (sourceColumn sourcePos)

located :: Parser a -> Parser (Located a)
located parser = do
   start <- location
   ret <- parser
   end <- location
   pure $ Located (LocRange start end) ret

program :: Parser Program
program = Program <$> (many $ located topLevelDecl) <* eof

topLevelDecl :: Parser TopLevelDecl
topLevelDecl =    (TLDFunc <$> funcDecl) 
              <|> (TLDClass <$> classDecl)
              <?> "top level declaration"

funcDecl :: Parser FuncDecl
funcDecl = do
    retType <- type_
    name <- ident
    args <- parens (commaSep funArg) <?> "arguments list"
    body <- braces (located stmt `sepEndBy` semi) <?> "function body"
    pure $ FuncDecl name args retType (StmtBlock body)

type_ :: Parser Type
type_ = do
    base <- baseType
    dims <- many . squares $ pure ()
    pure $ foldl (const . TyArray) base dims
  where
    baseType :: Parser Type
    baseType =     (reserved "int" >> pure TyInt)
               <|> (reserved "void" >> pure TyVoid)
               <|> (reserved "string" >> pure TyString)
               <|> (reserved "boolean" >> pure TyBool)
               <|> (TyClass <$> ident)
               <?> "base type"

funArg :: Parser FunArg
funArg = FunArg <$> type_ <*> ident

stmt :: Parser Stmt
stmt =    braces (StmtBlock <$> sepEndBy (located stmt) semi)
      <|> (reserved "return" >> StmtReturn <$> optionMaybe expr)
      <|> (reserved "if" >> StmtIf <$> located expr <*> located stmt <*> optionMaybe (located stmt))
      <|> (reserved "while" >> StmtWhile <$> parens (located expr) <*> located stmt)
      <|> (StmtDecl <$> localDecl)
      <|> exprBased
      <|> (semi >> pure StmtNone)
      <?> "statement"
  where
    exprBased = expr >>= \case
        e@(ExprLval !lval) -> choice
            [ reservedOp "++" >> pure (StmtInc lval)
            , reservedOp "--" >> pure (StmtDec lval)
            , reservedOp "=" >> StmtAssign lval <$> located expr
            , pure $ StmtExpr e
            ]
        e -> pure $ StmtExpr e

expr :: Parser Expr
expr = view obj <$> buildExpressionParser opsTable (located call)
  where
    call = basic >>= \case
        e@(ExprLval (LvalVar !name)) ->
            option e (ExprCall name <$> parens (commaSep $ located expr))
        e -> pure e

    basic =    (reserved "true" >> pure ExprTrue)
           <|> (reserved "false" >> pure ExprFalse)
           <|> (reserved "null" >> pure ExprNull)
           <|> int
           <|> (ExprString . BS.pack <$> stringLiteral)
           <|> new
    
    -- TODO
    int = ExprInt . fromInteger <$> integer

    new = do
        reserved "new"
        name <- ident
        option (ExprNew name) (ExprNewArr name <$> squares (located expr))

localDecl :: Parser LocalDecl
localDecl = LocalDecl <$> type_ <*> many localDeclItem <?> "declaration"

localDeclItem :: Parser LocalDeclItem
localDeclItem = LocalDeclItem <$> ident <*> optionMaybe (reservedOp "=" >> located expr)

classDecl :: Parser ClassDecl
classDecl = do
    reserved "class"
    name <- ident
    base <- optionMaybe (reserved "extends" >> ident)
    (fields, methods) <- partitionEithers <$> braces (many member)
    pure $ ClassDecl name base fields methods
  where
    member = try (Left <$> located field) <|> (Right <$> located funcDecl)
    field = ClassField <$> type_ <*> ident <* semi
