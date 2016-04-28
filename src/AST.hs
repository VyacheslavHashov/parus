module AST where

type Name = String

data Type = Void | Bool | I8 | I16 | I32 | I64
          | U8 | U16 | U32 | U64

data BinOpType = OpPlus | OpMinus | OpProduct | OpDivision
               | OpGt | OpGte | OpLe | OpLte | OpEq | OpNeq
               | OpNot | OpAnd | OpOr

data UnOpType = OpNot


data Expr = BinOp BinOpType Expr Expr
          | UnOp UnOpType Expr
          | FunApply Name [Expr]
          | Atom Int

data Instruction = VarDecl Name Type
                 | Assign Name Expr
                 | Return (Maybe Expr)
                 | IfBlock Expr CodeBlock
                 | IfElseBlock Expr CodeBlock CodeBlock
                 | WhileBlock Expr CodeBlock

type CodeBlock = [Instruction]

data ArgDecl = ArgDecl Name Type

data GlInstruction = GlVarDecl Name Type
                   | FunDecl Name [ArgDecl] CodeBlock Type

type Program = [GlInstruction]
