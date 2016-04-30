module Main where
import Lexer
import Parser

main = do
  s <- getContents
  putStrLn $ case scanner s of
    Right lst -> concatMap (\t -> "'" ++ show t ++ "' ") lst
    Left err -> "ERROR: " ++ err
