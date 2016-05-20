module AST where

import Data.Word
import qualified Data.Map as Map

-- Types

data PrimType = TVoid | TBool | TInt | TUint | TFloat
    deriving Show

data Type = ValType PrimType -- types of values
          | FunType [PrimType] PrimType -- list of argument's types
                                        -- and type of a returning value
          | ReturnType PrimType -- type of a return expression
    deriving Show

-- Operators

data BinOpType = OpPlus | OpMinus | OpProduct | OpDivision
               | OpGt | OpGte | OpLt | OpLte | OpEq | OpNeq
               | OpAnd | OpOr
    deriving Show

data UnOpType = OpNegate | OpNot
    deriving Show

type Name = String
type FunMap = Map.Map Name Function

-- AST
data AST = AST { globalVars :: Map.Map Name PrimType
               , functions :: FunMap
               }
    deriving Show

data Function = Function { fLocalVars :: Map.Map Name PrimType
                         , fReturnType :: PrimType
                         , fArgNames :: [Name]
                         , fCodeBlock :: CodeBlock
                         }
    deriving Show

type CodeBlock = [Instruction]

data Instruction = Assign Name Expr
                 | Return Expr
                 | IfElseBlock Expr CodeBlock CodeBlock
                 | WhileBlock Expr CodeBlock
                 | Expr Expr
    deriving Show

data Expr = BinOp BinOpType Expr Expr
          | UnOp UnOpType Expr
          | FunApply Name [Expr]
          | Ident Name
          | Value Value
          deriving Show

data Value = VoidValue
           | BoolValue Bool
           | IntValue Int
           | UintValue Word64
           | FloatValue Double
           | RawValue String
    deriving Show
-- Typed AST

data TypedAST = TypedAST { tGlobalVars :: Map.Map Name Type
                         , tFunctions :: Map.Map Name TFunction
                         }
    deriving Show

data TFunction = TFunction { tfType :: Type
                           , tfArgNames :: [Name]
                           , tfCodeBlock :: TCodeBlock
                           }
    deriving Show

type TCodeBlock = [TInstruction]

data TInstruction = TAssign Name TExpr
                  | TReturn TExpr
                  | TIfElseBlock TExpr TCodeBlock TCodeBlock
                  | TWhileBlock TExpr TCodeBlock
                  | TExpr TExpr
    deriving Show

data TExpr = TBinOp PrimType BinOpType TExpr TExpr
           | TUnOp PrimType UnOpType TExpr
           | TFunApply PrimType Name [TExpr]
           | TIdent PrimType Name
           | TValue PrimType TValue
          deriving Show

data TValue = TVoidValue
            | TBoolValue Bool
            | TIntValue Int
            | TUintValue Word64
            | TFloatValue Double
    deriving Show
