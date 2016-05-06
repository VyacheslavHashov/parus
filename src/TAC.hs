module TAC where

import Data.Word

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
            | And | Or
            | INeg | UNeg | FNeg
            | Not

type Label = String
data Var = Var String | Literal TacValue

data Tac = Tac Label TacInstruction

data TacInstruction = BinAssign Opcode Var Var Var
                    | UnAssign Opcode Var Var
                    | Copy Var Var
                    | Goto Label
                    | CondGoto Var Label
                    | Param Var
                    | Call Label
                    | Ret Var

data TacValue = TacBool Bool
              | TacInt Int
              | TacUint Word64
              | TacFloat Double


