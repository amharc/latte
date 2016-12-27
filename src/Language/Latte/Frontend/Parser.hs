{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Latte.Frontend.Parser where

import Control.Lens
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
    retType <- type_
    name <- ident
    args <- parens (commaSep funArg) <?> "arguments list"
    body <- braces (many $ located stmt) <?> "function body"
    pure $ FuncDecl name args retType (StmtBlock body)

type_ :: Parser Type
type_ = do
    base <- simpleType
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
funArg = FunArg <$> type_ <*> ident

stmt :: Parser Stmt
stmt =    braces (StmtBlock <$> many (located stmt))
      <|> (reserved "return" >> StmtReturn <$> optionMaybe (located expr) <* semi)
      <|> (reserved "if" >> StmtIf <$> parens (located expr) <*> located stmt 
           <*> optionMaybe (reserved "else" >>located stmt))
      <|> (reserved "while" >> StmtWhile <$> parens (located expr) <*> located stmt)
      <|> (do
            reserved "for"
            (ty, name, ex) <- parens ((,,) <$> type_ <*> ident <* colon <*> located expr)
            StmtFor ty name ex <$> located stmt)
      <|> try (StmtDecl <$> localDecl <* semi)
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
expr = view obj <$> buildExpressionParser opsTable (located cast)
  where
    cast = try (ExprCast <$> parens type_ <*> located call)
        <|> call

    call = located complexLval >>= \case
        Located l e@(ExprLval lval) ->
            option e (ExprCall (Located l lval) <$> parens (commaSep $ located expr))
        Located _ e -> pure e

    complexLval = do
        e <- located basic
        choice
            [ dot >> ExprLval . LvalField e <$> ident
            , ExprLval . LvalArray e <$> brackets (located expr)
            , pure $ e ^. obj
            ]

    basic =    (reserved "true" >> pure ExprTrue)
           <|> (reserved "false" >> pure ExprFalse)
           <|> (reserved "null" >> pure ExprNull)
           <|> int
           <|> (ExprString . BS.pack <$> stringLiteral)
           <|> new
           <|> parens expr
           <|> (ExprLval . LvalVar <$> ident)
    
    -- TODO
    int = ExprInt . fromInteger <$> integer

    new = do
        reserved "new"
        ty <- simpleType
        option (ExprNew ty) $ do
            dim <- brackets (located expr)
            dims <- many . brackets $ pure ()
            let ty' = foldl (const . TyArray) ty dims
            pure $ ExprNewArr ty' dim

localDecl :: Parser LocalDecl
localDecl = LocalDecl <$> type_ <*> (located localDeclItem `sepBy` comma) <?> "declaration"

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
    field = ClassField <$> type_ <*> ident <* semi
