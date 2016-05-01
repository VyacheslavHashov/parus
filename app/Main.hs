module Main where
import Lexer
import Parser

main = do
    a <- readFile "samples/fib.txt"
    let c = either (const []) id $ scanner a
    print $ program c

