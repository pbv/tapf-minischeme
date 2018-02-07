{-
  Symbolic expressions (a.k.a. S-expressions)
  Abstract syntax type, parsing and pretty printing  
-}
module Sexpr where

import           Text.Parsec
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language(emptyDef)
import           Data.List (foldl', intersperse)

type Identifier = String

-- | a datatype for S-expressions
data Sexpr
  = IntLit Integer
  | StrLit String
  | Atom Identifier
  | List [Sexpr]
  deriving Show

-- | concrete parser type
type Parser = Parsec String () 

-- | a parser for S-expressions
parserSexpr :: Parser Sexpr
parserSexpr =
  IntLit <$> integer
  <|>
  Atom <$> identifier
  <|>
  StrLit <$> stringLiteral
  <|>
  List <$> parens (many parserSexpr)


-- | lexer stuff
schemeDef
  = emptyDef { P.identStart = letter <|> symbol
             , P.identLetter = alphaNum <|> symbol
             , P.commentLine = ";"
             }

symbol :: Parser Char
symbol  = oneOf "!#$%&|*+-/:<=>?@^_~"

lexer = P.makeTokenParser schemeDef

identifier     = P.identifier lexer
natural        = P.natural lexer
stringLiteral  = P.stringLiteral lexer
parens         = P.parens lexer
whiteSpace      = P.whiteSpace lexer

integer :: Parser Integer
integer = (try (do { char '-'; negate <$> natural }) <|> natural) <?> "integer"


-- | pretty-printing
showsSexpr :: Sexpr -> ShowS 
showsSexpr (IntLit n) = shows n
showsSexpr (StrLit s) = shows s
showsSexpr (Atom x) = (x++)
showsSexpr (List xs) 
  = ('(':) .
    foldl' (.) id (intersperse (' ':) $ map showsSexpr xs) .
    (')':)

showSexpr :: Sexpr -> String
showSexpr e = showsSexpr e ""
