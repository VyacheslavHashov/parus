{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TAC where

import AST
import TypeCheck

import Data.Word
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State

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

data TacType = TacTypeVoid | TacTypeInt | TacTypeFloat
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
                    | GlDeclare TacName
                    | Nop
    deriving Show

data TacValue = TacBool Bool
              | TacInt Int
              | TacUint Word64
              | TacFloat Double
    deriving Show


printTacProgram :: TacProgram -> String
printTacProgram = unlines . map printTac
    where printTac (Tac mbl inst) = maybe "" (++":\n") mbl ++ printInst inst
          printInst = show


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

data TacGenEnv = TacGenEnv { teVarCounter :: Int
                           , teLabelCounter :: Int
                           , teNextLabel :: Maybe Label
                           }

type ExprGen = WriterT TacProgram TacGen

type TacGen = State TacGenEnv


initialEnv :: TacGenEnv
initialEnv = TacGenEnv { teVarCounter = 0
                       , teLabelCounter = 0
                       , teNextLabel = Nothing
                       }

mkTacProgram :: TypedAST -> TacProgram
mkTacProgram = concat . flip evalState initialEnv .
    traverse (tacCodeBlock . tfCodeBlock) . Map.elems . tFunctions

genTempName :: TacGen Name
genTempName = do
    env <- get
    let counter = teVarCounter env
        name = "_temp" ++ show counter
    put env{teVarCounter = counter + 1}
    pure name

genTempLabel :: TacGen Label
genTempLabel = do
    env <- get
    let counter = teLabelCounter env
        mbl     = teNextLabel env
    case mbl of
      Just label -> do
          put env{teNextLabel = Nothing}
          pure label
      _          -> do
          put env{teLabelCounter = counter + 1}
          pure $ "_L" ++ show counter

withLabel :: Label -> TacGen a -> TacGen a
withLabel lbl = withState $ \env -> env{teNextLabel = Just lbl}

putNextLabel :: Label -> TacGen ()
putNextLabel lbl = modify $ \env -> env{teNextLabel = Just lbl}

voidName :: TacName
voidName = "_void"

setLabelOnFirst :: Label -> TacProgram -> TacProgram
setLabelOnFirst label (Tac _ inst : xs) = Tac (Just label) inst : xs
setLabelOnFirst _ tp = tp

tacExpr :: TExpr -> TacGen (TacVar, TacProgram)
tacExpr = runWriterT . tacExprGen

tacExprGen :: TExpr -> ExprGen TacVar
tacExprGen (TBinOp pt op e1 e2) = do
    v1   <- tacExprGen e1
    v2   <- tacExprGen e2
    name <- lift genTempName
    let opcode = binOpToOpcode op
    tell [Tac Nothing $ BinAssign opcode name v1 v2]
    pure $ TacVar name
tacExprGen (TUnOp pt op e) = do
    v    <- tacExprGen e
    name <- lift genTempName
    let opcode = unOpToOpcode op
    tell [Tac Nothing $ UnAssign opcode name v]
    pure $ TacVar name
tacExprGen (TFunApply pt name es) = do
    args <- traverse tacExprGen es
    tell $ map (Tac Nothing . Param) args
    if pt == TVoid
       then tell [Tac Nothing $ CallVoid name] >>
            pure (TacVar voidName)
       else do
           rName <- lift genTempName
           tell [Tac Nothing $ Call rName name]
           pure $ TacVar rName
tacExprGen (TIdent pt name) = pure $ TacVar name
tacExprGen (TValue pt v) = pure . TacLiteral $ tacValue v

tacCodeBlock :: TCodeBlock -> TacGen TacProgram
tacCodeBlock = (concat <$>) . traverse (setLabel . tacInstruction)
    where
        setLabel :: TacGen TacProgram -> TacGen TacProgram
        setLabel pr = do
              env <- get
              case teNextLabel env of
                Just label -> do
                    put env{ teNextLabel = Nothing }
                    setLabelOnFirst label <$> pr
                _ -> pr


tacInstruction :: TInstruction -> TacGen TacProgram
tacInstruction (TAssign n e) = do
    (v, pr) <- tacExpr e
    pure $ pr ++ [Tac Nothing $ Copy (tacName n) v]

tacInstruction (TReturn e) = do
    (v, pr) <- tacExpr e
    let pt      = getPrimType e
        code    = case pt of
                     TVoid -> RetVoid
                     _     -> Ret v
    pure $ pr ++ [Tac Nothing code]

tacInstruction (TIfElseBlock e cb1 cb2) = do
    (v, pr) <- tacExpr e
    label1  <- genTempLabel
    label2  <- genTempLabel
    cb1'    <- withLabel label1 $ tacCodeBlock cb1
    cb2'    <- tacCodeBlock cb2
    putNextLabel label2
    pure $ pr ++
           [ Tac Nothing $ CondGoto v label1 ] ++
           cb2' ++
           [ Tac Nothing $ Goto label2 ] ++
           cb1'

tacInstruction (TWhileBlock e cb) = do
    (v, pr) <- tacExpr e
    label1  <- genTempLabel
    label2  <- genTempLabel
    cb'     <- tacCodeBlock cb
    v'      <- genTempName
    putNextLabel label2
    let pr' = setLabelOnFirst label1 $ pr ++ [Tac Nothing $ UnAssign BNot v' v]
    pure $ pr' ++
           [Tac Nothing $ CondGoto (TacVar v') label2] ++
           cb' ++
           [Tac Nothing $ Goto label1]

tacInstruction (TExpr e) = snd <$> tacExpr e

tacValue :: TValue -> TacValue
tacValue (TBoolValue v)  = TacBool v
tacValue (TIntValue v)   = TacInt v
tacValue (TUintValue v)  = TacUint v
tacValue (TFloatValue v) = TacFloat v
tacValue v = error $ "impossible happened in tacValue with value: "
                       ++ show v

tacName :: Name -> TacName
tacName = id
