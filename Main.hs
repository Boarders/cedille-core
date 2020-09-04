module Main where

import Parser.Util
import Parser.Parse
import Parser.AST


main :: IO ()
main = do
  input <- readFile "cdle/test.cdle"
  let parsedMod = runParser parseModule input
  let lexMod    = runLexer  input
  case parsedMod of
    Left err  -> print err
    Right ast -> putStrLn $ moduleToString ast
--  print lexMod
  
