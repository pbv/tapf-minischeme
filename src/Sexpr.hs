{-
  Symbolic expressions (a.k.a. S-expressions)
  Abstract syntax type, parsing and pretty printing  
-}
module Sexpr where

import           Text.Parsec 
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language(emptyDef)
import           Data.List (intercalate)

type Identifier = String

-- | a datatype for S-expressions
data Sexpr
  = Integer Integer
  | String String
  | Bool Bool
  | Atom Identifier
  | List [Sexpr]
  deriving Show


-- | Pretty-printer
-- not a very efficient solution
showSexpr :: Sexpr -> String
showSexpr (Integer n) = show n
showSexpr (String s) = show s
showSexpr (Atom x) = x
showSexpr (Bool b)
  = case b of
      True -> "#t"
      False -> "#f"
showSexpr (List list) 
  = "(" ++ intercalate " " (map showSexpr list) ++ ")"



-- | Parser 
type Parser = Parsec String () 

-- | S-expressions
parseSexpr :: Parser Sexpr
parseSexpr
  = parseNumber
  <|> parseString
  <|> parseAtomOrBool
  <|> parseList

parseList = List <$> parens (many parseSexpr)

parseNumber = Integer <$> integer

parseString = String <$> stringLiteral

parseAtomOrBool = do
  atom <- identifier 
  return $ case atom of
    "#f" -> Bool False
    "#t" -> Bool True
    _ -> Atom atom
  

-- | lexer language definitions 
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
integer = try $ do
  c <- option '+' (char '-' <|> char '+')
  n <- natural
  return (if c=='-' then negate n else n)
  

