module AST where

type Name = String

data Type = Void | Bool | I8 | I16 | I32 | I64
          | U8 | U16 | U32 | U64
          deriving Show

data BinOpType = OpPlus | OpMinus | OpProduct | OpDivision
               | OpGt | OpGte | OpLt | OpLte | OpEq | OpNeq
               | OpAnd | OpOr
               deriving Show

data UnOpType = OpNot
   deriving Show

----------------------------------
type Program = [GlInstruction]

data GlInstruction = GlVarDecl Name Type
                   | FunDecl Name [ArgDecl] CodeBlock Type
                   deriving Show

data ArgDecl = ArgDecl Name Type
    deriving Show

type CodeBlock = [Instruction]

data Instruction = VarDecl Name Type
                 | Assign Name Expr
                 | Return (Maybe Expr)
                 | IfBlock Expr CodeBlock
                 | IfElseBlock Expr CodeBlock CodeBlock
                 | WhileBlock Expr CodeBlock
                 deriving Show

data Expr = BinOp BinOpType Expr Expr
          | UnOp UnOpType Expr
          | FunApply Name [Expr]
          | Atom Int
          deriving Show





