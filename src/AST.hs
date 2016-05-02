module AST where

import Data.Word
import qualified Data.Map as Map

-- Types

data PrimType = TVoid | TBool | TInt | TUint | TFloat
    deriving Show

data Type = ValType PrimType -- types of values
          | FunType [PrimType] PrimType -- list of argument's types
                                        -- and type of a returning value
    deriving Show

-- Operators

data BinOpType = OpPlus | OpMinus | OpProduct | OpDivision
               | OpGt | OpGte | OpLt | OpLte | OpEq | OpNeq
               | OpAnd | OpOr
    deriving Show

data UnOpType = OpNegate | OpNot
    deriving Show

type Name = String

-- Raw AST
-- results after parsing

type RawProgram = [RawGlInstruction]

data RawGlInstruction = RawGlVarDecl Name PrimType
                      | RawFunDecl Name [RawArgDecl] RawCodeBlock PrimType
                   deriving Show

data RawArgDecl = RawArgDecl Name PrimType
    deriving Show

type RawCodeBlock = [RawInstruction]

data RawInstruction = RawVarDecl Name PrimType
                    | RawAssign Name RawExpr
                    | RawReturn (Maybe RawExpr)
                    | RawIfElseBlock RawExpr RawCodeBlock RawCodeBlock
                    | RawWhileBlock RawExpr RawCodeBlock
                    | RawExpr RawExpr
                    deriving Show

data RawExpr = RawBinOp BinOpType RawExpr RawExpr
             | RawUnOp UnOpType RawExpr
             | RawFunApply Name [RawExpr]
             | RawLiteral String
             | RawBoolLiteral Bool
             | RawIdent Name
          deriving Show

--
-- AST
--  after typechecking
--

data Value = VoidValue
           | BoolValue Bool
           | IntValue Int
           | UintValue Word64
           | FloatValue Double
    deriving Show

-- | Map of functions and global scope

type Scope = Map.Map Name Type
type FunTable = Map.Map Name Function

-- list of argument names
data Function = Function [Name] CodeBlock
    deriving Show

data AST = AST { funTable :: FunTable
               , glScope :: Scope
               }
    deriving Show

data CodeBlock = CodeBlock { scope :: Scope
                           , instructions :: [Instruction]
                           }
    deriving Show

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



