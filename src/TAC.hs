module TAC where

import AST
import Data.Word
import Data.Maybe
import Control.Monad.Writer

data Opcode = IPlus | UPlus | FPlus
            | IProduct | UProduct | FProduct
            | IMinus | UMinus | FMinus
            | FDivision
            | IGt | UGt | FGt
            | IGte | UGte | FGte
            | ILt | ULt | FLt
            | ILte | ULte | FLte
            | IEq | UEq | FEq
            | INeq | UNeq | FNeq
            | BAnd | BOr
            | INeg | UNeg | FNeg
            | BNot
    deriving Show

type TacProgram = [Tac]
type Label      = String
type TacName    = String

data TacVar = TacVar TacName | TacLiteral TacValue
    deriving Show

data Tac = Tac (Maybe Label) TacInstruction
    deriving Show

data TacInstruction = BinAssign Opcode TacName TacVar TacVar
                    | UnAssign Opcode TacName TacVar
                    | Copy TacName TacVar
                    | Goto Label
                    | CondGoto TacVar Label
                    | Param TacVar
                    | Call TacName Label
                    | CallVoid Label
                    | RetVoid
                    | Ret TacVar
                    | Declare TacName
    deriving Show

data TacValue = TacBool Bool
              | TacInt Int
              | TacUint Word64
              | TacFloat Double
    deriving Show

-- TODO: select opcode according to expression type
-- or put this selection in type-checking phase
binOpToOpcode :: BinOpType -> Opcode
binOpToOpcode t = case t of
    OpPlus     -> IPlus
    OpMinus    -> IMinus
    OpProduct  -> IProduct
    OpDivision -> FDivision
    OpGt       -> IGt
    OpGte      -> IGte
    OpLte      -> ILte
    OpEq       -> IEq
    OpNeq      -> INeq
    OpAnd      -> BAnd
    OpOr       -> BOr

unOpToOpcode :: UnOpType -> Opcode
unOpToOpcode t = case t of
    OpNegate -> INeg
    OpNot    -> BNot

type ExprGen = Writer TacProgram

mkTacProgram :: AST -> TacProgram
mkTacProgram = undefined

genTempName :: TacName
genTempName = "_temp"


tacExpr :: Expr -> (TacVar, TacProgram)
tacExpr = runWriter . tacExprGen

tacExprGen :: Expr -> ExprGen TacVar
tacExprGen (BinOp op e1 e2) = do
    v1 <- tacExprGen e1
    v2 <- tacExprGen e2
    let opcode = binOpToOpcode op
        name = genTempName
    tell [Tac Nothing $ BinAssign opcode name v1 v2]
    pure $ TacVar name
tacExprGen (UnOp op e) = do
    v <- tacExprGen e
    let opcode = unOpToOpcode op
        name = genTempName
    tell [Tac Nothing $ UnAssign opcode name v]
    pure $ TacVar name
tacExprGen (FunApply name es) = undefined
tacExprGen (Ident name) = pure $ TacVar name
tacExprGen (Value v) = pure . TacLiteral $ tacValue v

tacInstruction :: Instruction -> TacProgram
tacInstruction (Assign n e) = let (v, pr) = tacExpr e
                              in pr ++ [Tac Nothing $ Copy (tacName n) v]
tacInstruction (Return v) = undefined
tacInstruction (IfElseBlock v1 v2 v3) = undefined
tacInstruction (WhileBlock v1 v2) = undefined
tacInstruction (Expr v) = undefined

tacValue :: Value -> TacValue
tacValue (BoolValue v)  = TacBool v
tacValue (IntValue v)   = TacInt v
tacValue (UintValue v)  = TacUint v
tacValue (FloatValue v) = TacFloat v
tacValue v = error $ "impossible happened in tacValue with value: "
                       ++ show v

tacName :: Name -> TacName
tacName = id
