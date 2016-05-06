{-# LANGUAGE RankNTypes #-}
module Eval where

import Control.Monad.State
import qualified Data.Map as Map
import Debug.Trace

import AST

type EvalScope = Map.Map Name Value

data Env = Env { funcs :: FunTable
               , scopeStack :: [EvalScope]
               , currentScope :: EvalScope
               , globalScope :: EvalScope
               }


type Eval = State Env

changeVar :: Name -> Value -> Env -> Env
changeVar n v (Env fs scs sc gsc) =
    let (nSc, nGsc) = case (Map.lookup n sc, Map.lookup n gsc) of
                          (Just _, _) -> (Map.insert n v sc, gsc)
                          (Nothing, Just _) -> (sc, Map.insert n v gsc)
                          (Nothing, Nothing) -> error $ "not in scope" ++ n
    in Env fs scs nSc nGsc

pushScope :: EvalScope -> Env -> Env
pushScope nsc env@(Env _ scs sc _) = env { scopeStack = sc:scs
                                         , currentScope = nsc }

pullScope :: Env -> Env
pullScope env@(Env _ scs _  _) = env { scopeStack = tail scs
                                     , currentScope = head scs }

makeDefaultScope :: Function -> EvalScope
makeDefaultScope (Function lVars _ _ _) = getDefault <$> lVars
    where getDefault TVoid = VoidValue
          getDefault TBool = BoolValue False
          getDefault TInt = IntValue 0
          getDefault TUint = UintValue 0
          getDefault TFloat = FloatValue 0.0


runAST :: AST -> Value
runAST = flip evalState (Env Map.empty [] Map.empty Map.empty) . evalAST

evalAST :: AST -> Eval Value
evalAST (AST glVars funTable) = do
    put $ Env funTable [] Map.empty (const (error "gl scope") <$> glVars)
    evalExpr $ FunApply "main" []

evalCodeBlock :: CodeBlock -> Eval Value
evalCodeBlock insts =
    case insts of
        [] -> pure VoidValue
        _ -> last <$> mapM evalInstruction insts


evalInstruction :: Instruction -> Eval Value
evalInstruction (Assign name e) = do
    v <- evalExpr e
    modify (changeVar name v)
    pure VoidValue
evalInstruction (Return e) = evalExpr e
evalInstruction (IfElseBlock e cb1 cb2 ) = do
    b <- evalExpr e
    case b of
        BoolValue b -> if b
                         then evalCodeBlock cb1
                         else evalCodeBlock cb2
        _ -> error "non-boolean value in if-else expression"
evalInstruction (WhileBlock e cb) = do
    b <- evalExpr e
    case b of
        BoolValue b -> if b
                         then do evalCodeBlock cb
                                 evalInstruction (WhileBlock e cb)
                         else pure VoidValue
        _ -> error "non-boolean value in while expession"
evalInstruction (Expr e) = evalExpr e


evalExpr :: Expr -> Eval Value
evalExpr (BinOp optype e1 e2) = evalBinOp optype <$> evalExpr e1 <*> evalExpr e2
evalExpr (UnOp optype e) = evalUnOp optype <$> evalExpr e
evalExpr (FunApply name es) = do
    (Env funcs _ _ _) <- get
    case Map.lookup name funcs of
        Just func -> do
            args <- mapM evalExpr es
            let argsScope = Map.fromList $ zip (fArgNames func) args
            let scope = argsScope `Map.union` makeDefaultScope func
            modify $ pushScope scope
            v <- evalCodeBlock $ fCodeBlock func
            modify pullScope
            pure v
        Nothing -> error $ "undefined function : " ++ name
evalExpr (Ident name) = do
    env <- get
    case msum [Map.lookup name $ currentScope env,
               Map.lookup name $ globalScope env] of
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
            (v1,v2) -> error $ "type mismatch arOp" ++ show v1 ++ show v2
logArOp :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Value
logArOp op e1 e2 = case (e1, e2) of
            (IntValue v1, IntValue v2) -> BoolValue $ op v1 v2
            (UintValue v1, UintValue v2) -> BoolValue $ op v1 v2
            (FloatValue v1, FloatValue v2) -> BoolValue $ op v1 v2
            (v1,v2) -> error $ "type mismatch logArOp" ++ (show v1) ++ (show v2)
logOp op e1 e2 = case (e1, e2) of
            (BoolValue v1, BoolValue v2) -> BoolValue $ op v1 v2
            (_,_) -> error "type mismatch logOp"

evalUnOp :: UnOpType -> Value -> Value
evalUnOp OpNot (BoolValue b) = BoolValue $ not b
evalUnOp OpNot _ = error "type mismatch at not"
evalUnOp OpNegate (IntValue v) = IntValue (-v)
evalUnOp OpNegate (UintValue v) = UintValue (-v)
evalUnOp OpNegate (FloatValue v) = FloatValue (-v)

