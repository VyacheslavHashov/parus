{-# LANGUAGE RankNTypes #-}
module Eval where

import Control.Monad.State
import qualified Data.Map as Map
import Debug.Trace

import AST

type EvalScope = Map.Map Name Value

data Env = Env { funcs :: FunTable
               , scopeStack :: [EvalScope]
               }


type Eval = State Env

runAST :: AST -> Value
runAST = flip evalState (Env Map.empty []) . evalAST

evalAST :: AST -> Eval Value
evalAST (AST table scope) = do
    put $ Env table [const (error "gl scope") <$> scope]
    evalExpr $ FunApply "main" []

evalCodeBlock :: EvalScope -> CodeBlock -> Eval Value
evalCodeBlock newSc (CodeBlock scope insts) =
    case insts of
        [] -> pure VoidValue
        _ -> do
                (Env funcs scs) <- get
                let sc = Map.union newSc $ const undefined <$> scope
                put $ Env funcs (sc:scs)
                rs <- mapM evalInstruction insts
                (Env funcs (_:ss)) <- get
                put $ Env funcs ss
                pure $ last rs


evalInstruction :: Instruction -> Eval Value
evalInstruction (Assign name e) = do
    (Env funcs scs) <- get
    v <- evalExpr e
    let go [] = error $ "variable not in scope " ++ name
        go (x:xs) = case Map.lookup name x of
                          Just _ -> Map.insert name v x:xs
                          Nothing -> x:go xs
    let nScs = go scs
    put $ Env funcs nScs
    pure VoidValue
evalInstruction (Return e) = evalExpr e
evalInstruction (IfBlock e cb) = do
    b <- evalExpr e
    case b of
        BoolValue b -> if b
                         then evalCodeBlock Map.empty cb
                         else pure VoidValue
        _ -> error "non-boolean value in if expression"
evalInstruction (IfElseBlock e cb1 cb2 ) = do
    b <- evalExpr e
    case b of
        BoolValue b -> if b
                         then evalCodeBlock Map.empty cb1
                         else evalCodeBlock Map.empty cb2
        _ -> error "non-boolean value in if-else expression"
evalInstruction (WhileBlock e cb) = do
    b <- evalExpr e
    case b of
        BoolValue b -> if b
                         then do evalCodeBlock Map.empty cb
                                 evalInstruction (WhileBlock e cb)
                         else pure VoidValue
        _ -> error "non-boolean value in while expession"
evalInstruction (Expr e) = evalExpr e


evalExpr :: Expr -> Eval Value
evalExpr (BinOp optype e1 e2) = evalBinOp optype <$> evalExpr e1 <*> evalExpr e2
evalExpr (UnOp optype e) = evalUnOp optype <$> evalExpr e
evalExpr (FunApply name es) = do
    (Env funcs _) <- get
    case Map.lookup name funcs of
        Just (Function names cb) -> do
            args <- mapM evalExpr es
            let sc = Map.fromList $ zip names args
            evalCodeBlock sc cb
        Nothing -> error $ "undefined function : " ++ name
evalExpr (Ident name) = do
    (Env _ scs) <- get
    case msum $ map (Map.lookup name) scs of
        Just v -> pure v
        Nothing -> error $ "undefind variable: " ++ name
evalExpr (Value v) = pure v

evalBinOp :: BinOpType -> Value -> Value -> Value
evalBinOp OpPlus e1 e2 = arOp (+) e1 e2
evalBinOp OpMinus e1 e2 = arOp (-) e1 e2
evalBinOp OpProduct e1 e2 = arOp (*) e1 e2
evalBinOp OpDivision e1 e2 = case (e1, e2) of
                                (FloatValue v1, FloatValue v2) ->
                                    FloatValue $ v1 / v2

evalBinOp OpGt e1 e2 = logArOp (>) e1 e2
evalBinOp OpGte e1 e2 = logArOp (>=) e1 e2
evalBinOp OpLt e1 e2 = logArOp (<) e1 e2
evalBinOp OpLte e1 e2 = logArOp (<=) e1 e2
evalBinOp OpEq e1 e2 = logArOp (==) e1 e2
evalBinOp OpNeq e1 e2 = logArOp (/=) e1 e2
evalBinOp OpAnd e1 e2 = logOp (&&) e1 e2
evalBinOp OpOr e1 e2 = logOp (||) e1 e2

arOp :: (forall a. Num a => a -> a -> a) -> Value -> Value -> Value
arOp op e1 e2 = case (e1, e2) of
            (IntValue v1, IntValue v2) -> IntValue $ op v1 v2
            (UintValue v1, UintValue v2) -> UintValue $ op v1 v2
            (FloatValue v1, FloatValue v2) -> FloatValue $ op v1 v2
            (_,_) -> error "type mismatch arOp"
logArOp :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Value
logArOp op e1 e2 = case (e1, e2) of
            (IntValue v1, IntValue v2) -> BoolValue $ op v1 v2
            (UintValue v1, UintValue v2) -> BoolValue $ op v1 v2
            (FloatValue v1, FloatValue v2) -> BoolValue $ op v1 v2
            (_,_) -> error "type mismatch logArOp"
logOp op e1 e2 = case (e1, e2) of
            (BoolValue v1, BoolValue v2) -> BoolValue $ op v1 v2
            (_,_) -> error "type mismatch logOp"

evalUnOp :: UnOpType -> Value -> Value
evalUnOp OpNot (BoolValue b) = BoolValue $ not b
evalUnOp OpNot _ = error "type mismatch at not"
evalUnOp OpNegate (IntValue v) = IntValue (-v)
evalUnOp OpNegate (UintValue v) = UintValue (-v)
evalUnOp OpNegate (FloatValue v) = FloatValue (-v)

