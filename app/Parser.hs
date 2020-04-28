module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = Int <$> integer

floating :: Parser Expr
floating = Float <$> float

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [
  [
   binary "*" Ex.AssocLeft,
   binary "/" Ex.AssocLeft
  ]
 ,[
   binary "+" Ex.AssocLeft,
   binary "-" Ex.AssocLeft
  ]
 ,[binary "<" Ex.AssocLeft]
 ,[binary "=" Ex.AssocLeft]
  ]

expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

exprType :: Parser ExprType
exprType = do
  typeID <- identifier
  return $ case typeID of
    "int" -> IntType
    "float" -> FloatType

variable :: Parser Expr
variable = Var <$> identifier

definition :: Parser Expr
definition = do
  varType <- exprType
  whitespace
  varName <- identifier
  return $ Def varType varName

codeBlock :: Parser [Expr]
codeBlock = braces $ many $
  do e <- expr
     reserved ";"
     return e

decorator :: Parser Modifier
decorator = do
  char '@'
  Decorator <$> identifier

modifiers :: Parser [Modifier]
modifiers = do
  decs <- many decorator
  -- TODO: in future here we can add parsing of some other modifiers
  return decs

function :: Parser Expr
function = do
  mods <- modifiers
  funcType <- exprType
  name <- identifier
  args <- parens $ commaSep definition
  body <- codeBlock
  return $ Function mods funcType name args body

returnF :: Parser Expr
returnF = do
  reserved "return"
  e <- expr
  return $ Return e

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- parens expr
  tr <- codeBlock
  reserved "else"
  fl <- codeBlock
  return $ If cond tr fl

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try definition
      <|> try variable
      <|> try returnF
      <|> ifthen
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- function
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s = parse (contents toplevel) "<stdin>" s
