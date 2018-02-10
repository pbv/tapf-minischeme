{-
   Mini scheme interpreter
   Pedro Vasconcelos, 2018
-}

module Main where

import Control.Monad
import Text.Parsec
import Sexpr
import Eval

-- | main read-eval-print loop
main :: IO ()
main = do
  putStrLn banner
  loop
  where
    loop = do
      str <- getLine
      if null str then do
        putStrLn "Goodye!" 
        else do
          readEvalPrint str
          loop


readEvalPrint :: String -> IO ()
readEvalPrint str
  = case parse parseTopExpr "stdin" str of
      Left err -> print err
      Right expr -> do
        putStrLn ("> " ++ showSexpr expr)
        putStrLn ("= " ++ showSexpr (eval expr))


parseTopExpr :: Parser Sexpr
parseTopExpr = do
  whiteSpace
  e <- parseSexpr
  eof
  return e
 
 
banner :: String
banner =
  unlines [ "Mini scheme interpreter 0.1"
          , "Type S-expressions or an empty line to quit."
          ]
