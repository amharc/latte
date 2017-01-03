{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Latte.Frontend.Parser where

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as BS
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
    , P.nestedComments = True
    , P.identStart = letter
    , P.identLetter = alphaNum <|> oneOf "_'"
    , P.reservedNames = ["int", "void", "boolean", "string", "return", "for", "while", "new", "class", "extends", "null", "if", "else"]
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
    whiteSpace
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
    retType <- type_ True
    name <- ident
    args <- parens (commaSep $ located funArg) <?> "arguments list"
    body <- located (braces . many $ located (stmt True)) <?> "function body"
    pure $ FuncDecl name args retType (StmtBlock <$> body)

type_ :: Bool -> Parser Type
type_ voidAllowed = simpleType >>= \case
    TyVoid
        | voidAllowed -> pure TyVoid
        | otherwise -> unexpected "void"
    base -> do
        dims <- many . brackets $ pure ()
        pure $ foldl (const . TyArray) base dims

simpleType :: Parser Type
simpleType =     (reserved "int" >> pure TyInt)
             <|> (reserved "void" >> pure TyVoid)
             <|> (reserved "string" >> pure TyString)
             <|> (reserved "boolean" >> pure TyBool)
             <|> (TyClass <$> ident)
             <?> "base type"

funArg :: Parser FunArg
funArg = FunArg <$> type_ False <*> ident

stmt :: Bool -> Parser Stmt
stmt declAllowed =
          braces (StmtBlock <$> many (located $ stmt True))
      <|> (reserved "return" >> StmtReturn <$> optionMaybe (located expr) <* semi)
      <|> (reserved "if" >> StmtIf <$> parens (located expr) <*> located (stmt False)
           <*> optionMaybe (reserved "else" >> located (stmt False)))
      <|> (reserved "while" >> StmtWhile <$> parens (located expr) <*> located (stmt False))
      <|> (do
            reserved "for"
            (ty, name, ex) <- parens ((,,) <$> type_ False <*> ident <* colon <*> located expr)
            StmtFor ty name ex <$> located (stmt False))
      <|> (if declAllowed then try (StmtDecl <$> localDecl <* semi) else unexpected "declaration")
      <|> (exprBased <* semi)
      <|> (semi >> pure StmtNone)
      <?> "statement"
  where
    exprBased = located expr >>= \case
        Located l e@(ExprLval !lval) -> choice
            [ reservedOp "++" >> pure (StmtInc $ Located l lval)
            , reservedOp "--" >> pure (StmtDec $ Located l lval)
            , reservedOp "=" >> StmtAssign (Located l lval) <$> located expr
            , pure $ StmtExpr e
            ]
        Located _ e -> pure $ StmtExpr e

expr :: Parser Expr
expr = (view obj <$> buildExpressionParser opsTable (located cast)) <?> "expression"
  where
    cast = try (ExprCast <$> parens (type_ False) <*> located cast)
        <|> complex

    complex = view obj <$> (located basic >>= go)
      where
        go :: Located Expr -> Parser (Located Expr)
        go acc = choice
            [ meld acc (dot >> ExprLval . LvalField acc <$> ident) >>= go
            , meld acc (ExprLval . LvalArray acc <$> brackets (located expr)) >>= go
            , (case acc of
                Located l (ExprLval lval) ->
                    meld acc (ExprCall (Located l lval) <$> parens (commaSep $ located expr)) >>= go
                _ -> unexpected "function call")
            , pure acc
            ]

        meld acc parser = do
            x <- located parser
            pure (Located (acc ^. loc <> x ^. loc) $ x ^. obj)

    basic =    (reserved "true" >> pure ExprTrue)
           <|> (reserved "false" >> pure ExprFalse)
           <|> (reserved "null" >> pure ExprNull)
           <|> int
           <|> (ExprString . BS.pack <$> stringLiteral)
           <|> new
           <|> parens expr
           <|> (ExprLval . LvalVar <$> ident)
    
    int = do
        val <- integer
        when (val < intMinBound || val > intMaxBound) $ unexpected "integer overflow"
        pure . ExprInt $ fromInteger val

    intMinBound = - 2 ^ 31
    intMaxBound = 2 ^ 31 - 1

    new = do
        reserved "new"
        ty <- simpleType
        option (ExprNew ty) $ do
            dim <- brackets (located expr)
            dims <- many . brackets $ pure ()
            let ty' = foldl (const . TyArray) ty dims
            pure $ ExprNewArr ty' dim

localDecl :: Parser LocalDecl
localDecl = LocalDecl <$> type_ False <*> (located localDeclItem `sepBy` comma) <?> "declaration"

localDeclItem :: Parser LocalDeclItem
localDeclItem = LocalDeclItem <$> ident <*> optionMaybe (reservedOp "=" >> located expr)

classDecl :: Parser ClassDecl
classDecl = do
    reserved "class"
    name <- ident
    base <- optionMaybe (reserved "extends" >> ident)
    members <- braces . many $ located member
    pure $ ClassDecl name base members
  where
    member = try (ClassMemberField <$> field)
             <|> (ClassMemberMethod <$> funcDecl)
    field = ClassField <$> type_ False <*> ident <* semi
