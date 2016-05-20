module Main where
import Control.Monad.Except
import Lexer
import Parser
import TypeCheck
import Eval
import TAC
import AST

main = do
    a <- readFile "samples/fib.txt"
    let c = either (const []) id $ scanner a
    print $ program c

getAST = do
    a <- readFile "samples/factorial.txt"
    let c = either (const []) id $ scanner a
        d = either undefined id $ runExcept $ program c
    pure d

expr :: Expr
expr = BinOp OpPlus (Ident "a") (BinOp OpPlus (Ident "b") (Ident "c"))

