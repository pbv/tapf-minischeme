

module Main where

import Control.Monad
import Text.Parsec
import Sexpr

main :: IO ()
main = do
  putStrLn banner
  readLoop

banner :: String
banner = unlines [ "Mini scheme interpreter 0.1;"
                 , "Type S-expressions or an empty line to quit."
                 ]


readLoop :: IO ()
readLoop = do
  putStr "> "
  str <- getLine
  if null str then do
    putStrLn "Goodye!" 
    else do
      readPrint str
      readLoop

readPrint :: String -> IO ()
readPrint str
  = case parse parserToplevel "stdin" str of
      Left err -> print err
      Right expr -> putStrLn (showSexpr expr)


parserToplevel :: Parser Sexpr
parserToplevel = do
  whiteSpace
  e <- parserSexpr
  eof
  return e
  

    

