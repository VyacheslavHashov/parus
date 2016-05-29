module AST where

import Data.Word
import qualified Data.Map as Map


-- Names

newtype VarName  = VarName String
    deriving (Show, Eq, Ord)
newtype FunName = FunName String
    deriving (Show, Eq, Ord)

-- Types

data Type = TVoid | TBool | TInt | TUint | TFloat
    deriving (Show, Eq)

data FunType = FunType [Type] Type
    deriving (Show, Eq)

-- | possible types of overloaded operator or literal
type PolyType = [Type]

-- Operators

data BinOpType
    = OpPlus | OpMinus | OpProduct | OpDivision
    | OpGt | OpGte | OpLt | OpLte | OpEq | OpNeq
    | OpAnd | OpOr
    deriving Show

data UnOpType = OpNegate | OpNot
    deriving Show

-- Raw AST

data AST = AST
    { globalVars :: Map.Map VarName Type
    , functions :: Map.Map FunName Function
    } deriving Show

data Function = Function
    { fLocalVars :: Map.Map VarName Type -- args too
    , fReturnType :: Type
    , fArgNames :: [VarName]
    , fCodeBlock :: CodeBlock
    } deriving Show

type CodeBlock = [Instruction]

data Instruction
    = Assign VarName Expr
    | Return Expr
    | IfElseBlock Expr CodeBlock CodeBlock
    | WhileBlock Expr CodeBlock
    | Expr Expr
    deriving Show

data Expr
    = BinOp BinOpType Expr Expr
    | UnOp UnOpType Expr
    | FunApply FunName [Expr]
    | Ident VarName
    | Value Value
    deriving Show

data Value
    = VoidValue
    | BoolValue Bool
    | RawValue String
    deriving Show

-- Typed AST

data TypedAST = TypedAST
    { tGlobalVars :: Map.Map VarName (Type, TValue)
    , tFunctions :: Map.Map FunName TFunction
    } deriving Show

data TFunction = TFunction
    { tfType :: Type
    , tfArgNames :: [VarName]
    , tfCodeBlock :: TCodeBlock
    } deriving Show

type TCodeBlock = [TInstruction]

data TInstruction
    = TAssign VarName TExpr
    | TReturn TExpr
    | TIfElseBlock TExpr TCodeBlock TCodeBlock
    | TWhileBlock TExpr TCodeBlock
    | TExpr TExpr
    deriving Show

data TExpr
    = TBinOp Type BinOpType TExpr TExpr
    | TUnOp Type UnOpType TExpr
    | TFunApply Type FunName [TExpr]
    | TIdent Type VarName
    | TValue Type TValue
    deriving Show

data TValue
    = TVoidValue
    | TBoolValue Bool
    | TIntValue Int
    | TUintValue Word64
    | TFloatValue Double
    deriving Show

isReturnStmt :: TInstruction -> Bool
isReturnStmt (TReturn _) = True
isReturnStmt _           = False
