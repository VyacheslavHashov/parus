module TypeCheck where

import Control.Monad.Except
import Data.List
import qualified Data.Map as Map

import AST


data TypeError = TypeMismatch Type Type
               | NotInScopeFun Name
               | NotInScopeVar Name
               | DoubleDecl Name

type Check = Except TypeError

type TypeScope = Map.Map Name Type

typeOfValue :: Value -> Type
typeOfValue VoidValue = ValType TVoid
typeOfValue (BoolValue _) = ValType TBool
typeOfValue (IntValue _) = ValType TInt
typeOfValue (UintValue _) = ValType TUint
typeOfValue (FloatValue _) = ValType TFloat

mkGlTypeScope :: RawProgram -> TypeScope
mkGlTypeScope = Map.fromList . map toScope
    where toScope (RawGlVarDecl name pt) = (name, ValType pt)
          toScope (RawFunDecl name args _ pt) =
            (name, FunType (map (\(RawArgDecl _ pt) -> pt) args) pt)

-- | Transforms to cope AST without typechecking
--
mkAST :: RawProgram -> AST
mkAST program = let (scope, table) = go [] [] program
                in AST (Map.fromList table) (Map.fromList scope)
    where
        go :: [(Name, Type)] -> [(Name, Function)] -> RawProgram ->
              ([(Name, Type)] , [(Name, Function)])
        go scope table [] = (scope , table)
        go scope table (x:xs) = case x of
            (RawGlVarDecl name pt) -> go ((name, ValType pt):scope) table xs
            (RawFunDecl name args cb pt) ->
                go scope ((name, Function (getArgNames args)
                                 (mkCodeBlock cb args)):table) xs
        getArgNames :: [RawArgDecl] -> [Name]
        getArgNames = map (\(RawArgDecl name _) -> name)

mkCodeBlock :: RawCodeBlock -> [RawArgDecl] -> CodeBlock
mkCodeBlock cb args = let scope = map fromArg args
                          (scope', insts) = go scope [] cb
                      in CodeBlock (Map.fromList scope') (reverse insts)
    where
        fromArg :: RawArgDecl -> (Name, Type)
        fromArg (RawArgDecl name pt) = (name, ValType pt)

        go :: [(Name, Type)] -> [Instruction] -> [RawInstruction]
           -> ([(Name, Type)] , [Instruction])
        go scope insts [] = (scope, insts)
        go scope insts (x:xs) = case x of
            (RawVarDecl name pt) -> go ((name, ValType pt):scope) insts xs
            (RawAssign name expr) -> go scope (Assign name (mkExpr expr):insts) xs
            (RawReturn rexpr) -> case rexpr of
                Nothing -> go scope (Return (Value VoidValue):insts) xs
                (Just expr) -> go scope (Return (mkExpr expr):insts) xs
            (RawIfElseBlock expr cb1 cb2) -> go scope (IfElseBlock (mkExpr expr)
                                             (mkCodeBlock cb1 [])
                                             (mkCodeBlock cb2 []):insts) xs
            (RawWhileBlock expr cb) -> go scope (WhileBlock (mkExpr expr)
                                        (mkCodeBlock cb []):insts) xs
            (RawExpr expr) -> go scope (Expr (mkExpr expr):insts) xs

mkExpr :: RawExpr -> Expr
mkExpr (RawBinOp optype e1 e2) = BinOp optype (mkExpr e1) (mkExpr e2)
mkExpr (RawUnOp optype e) = UnOp optype (mkExpr e)
mkExpr (RawFunApply name exprs) = FunApply name (map mkExpr exprs)
mkExpr (RawLiteral s) = Value . IntValue $ read s
mkExpr (RawBoolLiteral b) = Value $ BoolValue b
mkExpr (RawIdent name) = Ident name




















