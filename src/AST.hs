module AST where

-- Types

data PrimType = TVoid | TBool | TInt | TUint | TFloat
    deriving Show

data Type = ValType PrimType -- types of values
          | FunType [PrimType] PrimType -- list of argument's types
                                        -- and type of a returning value

-- Operators

data BinOpArType = OpPlus | OpMinus | OpProduct | OpDivision
    deriving Show

data BinOpLogType = OpGt | OpGte | OpLt | OpLte | OpEq | OpNeq
                  | OpAnd | OpOr
    deriving Show

data UnOpLogType = OpNot
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
                    | RawIfBlock RawExpr RawCodeBlock
                    | RawIfElseBlock RawExpr RawCodeBlock RawCodeBlock
                    | RawWhileBlock RawExpr RawCodeBlock
                    deriving Show

data RawExpr = RawBinArOp BinOpArType RawExpr RawExpr
             | RawBinLogOp BinOpLogType RawExpr RawExpr
             | RawUnOp UnOpLogType RawExpr
             | RawFunApply Name [RawExpr]
             | RawLiteral String
             | RawIdent String
          deriving Show


-- data Value = VoidValue ()
--            | BoolValue Bool
--            | IntValue Int
--            | UintValue Word64
--            | FloatValue Double





