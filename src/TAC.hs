module TAC where

import AST
import TypeCheck

import Data.Word
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State

data Opcode
    = IPlus | UPlus | FPlus
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

newtype Label   = Label String
    deriving (Show, Eq, Ord)
newtype TacName = TacName String
    deriving (Show, Eq, Ord)
type TacList    = [Tac]

data TacVar = TacVar TacName | TacLiteral TacValue
    deriving Show

data TacType = TacTypeVoid | TacTypeInt | TacTypeFloat
    deriving Show

data Tac = Tac (Maybe Label) TacInstruction
    deriving Show

data TacInstruction
    = BinAssign Opcode TacName TacVar TacVar
    | UnAssign Opcode TacName TacVar
    | Copy TacName TacVar
    | Goto Label
    | CondGoto TacVar Label
    | Call TacName [TacVar] Label
    | CallVoid [TacVar] Label
    | RetVoid
    | Ret TacVar
    | Nop
    deriving Show

data TacValue
    = TacBool Bool
    | TacInt Int
    | TacUint Word64
    | TacFloat Double
    deriving Show

data TacProgram = TacProgram
    { tacGlobals :: Map.Map TacName TacValue
    , tacFunctions :: Map.Map Label TacFunction
    }
    deriving Show

data TacFunction = TacFunction
    { tacfName :: Label
    , tacfArgs :: [TacName]
    , tacfCode :: TacList
    }
    deriving Show

data TacGenEnv = TacGenEnv
    { teVarCounter :: Int
    , teLabelCounter :: Int
    , teNextLabel :: Maybe Label
    }

type ExprGen = WriterT TacList TacGen

type TacGen = State TacGenEnv

initialEnv :: TacGenEnv
initialEnv = TacGenEnv
    { teVarCounter = 0
    , teLabelCounter = 0
    , teNextLabel = Nothing
    }

freshName :: TacGen TacName
freshName = do
    env <- get
    let counter = teVarCounter env
        name = TacName $ "_temp" ++ show counter
    put env{teVarCounter = counter + 1}
    pure name

freshLabel :: TacGen Label
freshLabel = do
    env <- get
    let counter = teLabelCounter env
        mbl     = teNextLabel env
    case mbl of
      Just label -> do
          put env{teNextLabel = Nothing}
          pure label
      _          -> do
          put env{teLabelCounter = counter + 1}
          pure . Label $ "_L" ++ show counter

withLabel :: Label -> TacGen a -> TacGen a
withLabel lbl = withState $ \env -> env{teNextLabel = Just lbl}

putNextLabel :: Label -> TacGen ()
putNextLabel lbl = modify $ \env -> env{teNextLabel = Just lbl}

tacProgram :: TypedAST -> TacProgram
tacProgram ast = TacProgram
    { tacGlobals = globals
    , tacFunctions = functions
    }
  where
      globals   = Map.fromList . (mkGlobal <$>) . Map.toList $ tGlobalVars ast
      functions = Map.fromList . flip evalState initialEnv .
          traverse mkFun . Map.toList $ tFunctions ast
      mkFun :: (FunName, TFunction) -> TacGen (Label, TacFunction)
      mkFun (name, func) = (,) (toLabel name) <$> tacFunction name func
      mkGlobal :: (VarName, (Type, TValue)) -> (TacName, TacValue)
      mkGlobal (name, (_, v)) = (toTacName name, tacValue v)

tacFunction :: FunName -> TFunction -> TacGen TacFunction
tacFunction name func = do
    codeblock <- tacCodeBlock $ tfCodeBlock func
    pure TacFunction
        { tacfName = toLabel name
        , tacfArgs = map toTacName $ tfArgNames func
        , tacfCode = codeblock
        }

tacCodeBlock :: TCodeBlock -> TacGen TacList
tacCodeBlock [] = setLabel $ pure [Tac Nothing Nop]
tacCodeBlock cb = concat <$> traverse (setLabel . tacInstruction) cb

setLabel :: TacGen TacList -> TacGen TacList
setLabel pr = do
      env <- get
      case teNextLabel env of
        Just label -> do
            put env{ teNextLabel = Nothing }
            setLabelOnFirst label <$> pr
        _ -> pr

setLabelOnFirst :: Label -> TacList -> TacList
setLabelOnFirst label (Tac _ inst : xs) = Tac (Just label) inst : xs
setLabelOnFirst _ tp = tp

tacInstruction :: TInstruction -> TacGen TacList
tacInstruction (TAssign n e) = do
    (v, pr) <- tacExpr e
    pure $ pr ++ [Tac Nothing $ Copy (toTacName n) v]

tacInstruction (TReturn e) = do
    (v, pr) <- tacExpr e
    let pt      = getType e
        code    = case pt of
                     TVoid -> RetVoid
                     _     -> Ret v
    pure $ pr ++ [Tac Nothing code]

-- | if expr cb1 cb2 =>
--      t = expr
--      condgoto t L1
--      cb2
--      goto L2
--  L1:
--      cb1
--  L2:
--      ...
tacInstruction (TIfElseBlock e cb1 cb2) = do
    (v, pr) <- tacExpr e
    label1  <- freshLabel
    label2  <- freshLabel
    cb1'    <- withLabel label1 $ tacCodeBlock cb1
    cb2'    <- tacCodeBlock cb2
    putNextLabel label2
    pure $ pr ++
           [ Tac Nothing $ CondGoto v label1 ] ++
           cb2' ++
           [ Tac Nothing $ Goto label2 ] ++
           cb1'

-- | while expr cb =>
--  L1:
--      t = expr
--      t2 = NOT t
--      condgoto t2 L2
--      cb
--      goto L1
--  L2:
--      ...
tacInstruction (TWhileBlock e cb) = do
    (v, pr) <- tacExpr e
    label1  <- freshLabel
    label2  <- freshLabel
    cb'     <- tacCodeBlock cb
    v'      <- freshName
    putNextLabel label2
    let pr' = setLabelOnFirst label1 $ pr ++ [Tac Nothing $ UnAssign BNot v' v]
    pure $ pr' ++
           [Tac Nothing $ CondGoto (TacVar v') label2] ++
           cb' ++
           [Tac Nothing $ Goto label1]

tacInstruction (TExpr e) = snd <$> tacExpr e

tacExpr :: TExpr -> TacGen (TacVar, TacList)
tacExpr = runWriterT . tacExprGen

tacExprGen :: TExpr -> ExprGen TacVar
tacExprGen (TBinOp pt op e1 e2) = do
    v1   <- tacExprGen e1
    v2   <- tacExprGen e2
    name <- lift freshName
    let opcode = binOpToOpcode op
    tell [Tac Nothing $ BinAssign opcode name v1 v2]
    pure $ TacVar name
tacExprGen (TUnOp pt op e) = do
    v    <- tacExprGen e
    name <- lift freshName
    let opcode = unOpToOpcode op
    tell [Tac Nothing $ UnAssign opcode name v]
    pure $ TacVar name
tacExprGen (TFunApply pt name es) = do
    args <- traverse tacExprGen es
    if pt == TVoid
       then tell [Tac Nothing $ CallVoid args (toLabel name)] >>
            pure (TacVar voidName)
       else do
           rName <- lift freshName
           tell [Tac Nothing $ Call rName args (toLabel name)]
           pure $ TacVar rName
tacExprGen (TIdent pt name) = pure . TacVar $ toTacName name
tacExprGen (TValue pt v) = pure . TacLiteral $ tacValue v

tacValue :: TValue -> TacValue
tacValue (TBoolValue v)  = TacBool v
tacValue (TIntValue v)   = TacInt v
tacValue (TUintValue v)  = TacUint v
tacValue (TFloatValue v) = TacFloat v
tacValue v = error $ "impossible happened in tacValue with value: "
                       ++ show v

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

-- Utils
--
printTacList :: TacList -> String
printTacList = unlines . map printTac
    where printTac (Tac mbl inst) = maybe "" ((++":\n").show) mbl ++
                                    printInst inst
          printInst = ("    " ++) . show

toTacName :: VarName -> TacName
toTacName (VarName n) = TacName n

toLabel :: FunName -> Label
toLabel (FunName n) = Label n

voidName :: TacName
voidName = TacName "_void"
