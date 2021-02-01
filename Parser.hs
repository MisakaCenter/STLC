module Parser where

import qualified Data.Functor.Identity
import STLC (Tm (..), Ty (TyArrow, TyNat))
import qualified Text.Parsec as Text.Parsec.Prim
import Text.ParserCombinators.Parsec
  ( Parser,
    alphaNum,
    char,
    letter,
    parse,
    string,
    (<|>),
  )
import Text.ParserCombinators.Parsec.Expr ()
import Text.ParserCombinators.Parsec.Language
  ( GenLanguageDef,
    emptyDef,
  )
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
  emptyDef
    { Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames =
        [ "var",
          "app",
          "abs",
          "const",
          "var"
        ],
      Token.reservedOpNames =
        [ "->",
          "=>",
          "."
        ]
    }

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

identifier :: Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity String
identifier = Token.identifier lexer

reserved :: String -> Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity ()
reserved = Token.reserved lexer

reservedOp :: String -> Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity ()
reservedOp = Token.reservedOp lexer

parens :: Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity a -> Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity a
parens = Token.parens lexer

whiteSpace :: Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity ()
whiteSpace = Token.whiteSpace lexer

stlcParser :: Parser Tm
stlcParser = whiteSpace >> statement

tyParser :: Parser Ty
tyParser = parens tyParser_

tyParser_ :: Parser Ty
tyParser_ = arrowTy <|> natTy

natTy :: Parser Ty
natTy =
  do
    string "Nat"
    return TyNat

arrowTy :: Parser Ty
arrowTy =
  do
    ty1 <- tyParser
    reserved "->"
    TyArrow ty1 <$> tyParser

statement :: Parser Tm
statement =
  parens statement
    <|> statement'

statement' :: Parser Tm
statement' =
  varTm
    <|> constTm
    <|> appTm
    <|> absTm

varTm :: Parser Tm
varTm =
  do
    reserved "var"
    Var <$> identifier

constTm :: Parser Tm
constTm =
  do
    reserved "const"
    Const <$> identifier

appTm :: Parser Tm
appTm =
  do
    reserved "app"
    app1 <- statement
    App app1 <$> statement

absTm :: Parser Tm
absTm =
  do
    reserved "abs"
    char '.'
    varName <- identifier
    char ':'
    tyName <- tyParser
    char '='
    char '>'
    Abs varName tyName <$> statement