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

mkAST :: AST -> AST
mkAST ast = ast { functions = mkFunction <$> functions ast }

mkFunction :: Function -> Function
mkFunction func = func { fCodeBlock = mkCodeBlock $ fCodeBlock func }

mkCodeBlock :: CodeBlock -> CodeBlock
mkCodeBlock = map mkInstruction

mkInstruction :: Instruction -> Instruction
mkInstruction (Assign name e) = Assign name $ mkExpr e
mkInstruction (Return e) = Return $ mkExpr e
mkInstruction (IfElseBlock e cb1 cb2) = IfElseBlock (mkExpr e)
                                        (mkCodeBlock cb1) (mkCodeBlock cb2)
mkInstruction (WhileBlock e cb) = WhileBlock (mkExpr e) (mkCodeBlock cb)
mkInstruction (Expr e) = Expr $ mkExpr e

mkExpr :: Expr -> Expr
mkExpr (BinOp op e1 e2) = BinOp op (mkExpr e1) (mkExpr e2)
mkExpr (UnOp op e) = UnOp op $ mkExpr e
mkExpr (FunApply name es) = FunApply name $ map mkExpr es
mkExpr (Ident n) = Ident n
mkExpr (Value v) = Value $ mkValue v

mkValue :: Value -> Value
mkValue (RawValue lit) = IntValue $ read lit
mkValue v = v




















