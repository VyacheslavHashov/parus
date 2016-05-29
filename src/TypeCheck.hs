module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Word
import Data.Maybe
import qualified Data.Map as Map

import AST


data TypeError
    = TypeMismatch Type Type
    | PolyTypeMismatch Type PolyType
    | UnresolvablePolyTypes PolyType PolyType
    | NotInScopeFun FunName
    | NotInScopeVar VarName
    | NoReturn
    | MissingMain
    deriving Show

class HasType a where
    getType :: a -> Type

instance HasType TExpr where
    getType (TBinOp t _ _ _)  = t
    getType (TUnOp t _ _)     = t
    getType (TFunApply t _ _) = t
    getType (TIdent t _)      = t
    getType (TValue t _)      = t

instance HasType TValue where
    getType TVoidValue      = TVoid
    getType (TBoolValue _)  = TBool
    getType (TIntValue _)   = TInt
    getType (TUintValue _)  = TUint
    getType (TFloatValue _) = TFloat

defaultValue :: Type -> TValue
defaultValue TVoid  = TVoidValue
defaultValue TBool  = TBoolValue False
defaultValue TInt   = TIntValue 0
defaultValue TUint  = TUintValue 0
defaultValue TFloat = TFloatValue 0.0

mainName :: FunName
mainName = FunName "main"

numericTypes :: PolyType
numericTypes = [TInt , TUint , TFloat]

boolAndNumericTypes :: PolyType
boolAndNumericTypes = [TBool , TInt , TUint , TFloat]

toPolyType :: Type -> PolyType
toPolyType t = [t]

data CheckEnv = CheckEnv
    { ceGlobalVars :: Map.Map VarName Type
    , ceFunctions :: Map.Map FunName FunType
    , ceLocalVars :: Map.Map VarName Type
    , ceReturnType :: Type
    }

type Check = ReaderT CheckEnv (Except TypeError)

lookupVar :: VarName -> Check Type
lookupVar name = do
    env <- ask
    case msum [ Map.lookup name $ ceLocalVars env
              , Map.lookup name $ ceGlobalVars env] of
        Just t -> pure t
        _      -> throwError $ NotInScopeVar name

lookupFunction :: FunName -> Check FunType
lookupFunction name = do
    mbt <- asks $ Map.lookup name . ceFunctions
    case mbt of
        Just t -> pure t
        _      -> throwError $ NotInScopeFun name

polyTypeCheck :: Type -> PolyType -> Check a -> Check a
polyTypeCheck t pt e = when (t `notElem` pt) (throwError err) >> e
  where err = PolyTypeMismatch t pt

-- TODO move this function in another place
mkFunctionType :: Function -> FunType
mkFunctionType f =
    let rType    = fReturnType f
        argTypes = map (fLocalVars f Map.!) $ fArgNames f
    in FunType argTypes rType


typeAST :: AST -> Except TypeError TypedAST
typeAST ast = do
    let gVars  = globalVars ast
        fTypes = mkFunctionType <$> functions ast
    fs <- traverse (typeFunction gVars fTypes) $ functions ast
    when (mainName `Map.notMember` functions ast) $ throwError MissingMain
    pure TypedAST { tGlobalVars = ((,) <*> defaultValue) <$> gVars
                  , tFunctions = fs }

typeFunction :: Map.Map VarName Type ->
                Map.Map FunName FunType ->
                Function ->
                Except TypeError TFunction
typeFunction gVars fTypes func = do
    let localTypes = fLocalVars func
        currType   = fReturnType func
        checkEnv   = CheckEnv { ceGlobalVars  = gVars
                              , ceFunctions = fTypes
                              , ceLocalVars = localTypes
                              , ceReturnType = currType
                              }

    codeBlock <- runReaderT (checkFunCodeBlock $ fCodeBlock func) checkEnv
    pure TFunction { tfType = currType
                   , tfArgNames = fArgNames func
                   , tfCodeBlock = codeBlock
                   }

-- Check here if return statement exists in codeblock
checkFunCodeBlock :: CodeBlock -> Check TCodeBlock
checkFunCodeBlock cb = do
    xs <- traverse (fmap f . checkInstruction) cb
    when (True `notElem` map snd xs) $ throwError NoReturn
    pure $ map fst xs
  where f inst = (inst, isReturnStmt inst)

checkCodeBlock :: CodeBlock -> Check TCodeBlock
checkCodeBlock = traverse checkInstruction

checkInstruction :: Instruction -> Check TInstruction
checkInstruction (Assign name e) = do
    t <- lookupVar name
    (pt, evalE) <- checkExpr e
    polyTypeCheck t pt . pure $ TAssign name (evalE t)

checkInstruction (Return e) = do
    retType     <- asks ceReturnType
    (pt, evalE) <- checkExpr e
    polyTypeCheck retType pt . pure $ TReturn (evalE retType)

checkInstruction (IfElseBlock e cb1 cb2) = do
    (pte, evalE) <- checkExpr e
    polyTypeCheck TBool pte $
        TIfElseBlock (evalE TBool) <$> checkCodeBlock cb1
                                   <*> checkCodeBlock cb2

checkInstruction (WhileBlock e cb) = do
    (pte, evalE) <- checkExpr e
    polyTypeCheck TBool pte $
        TWhileBlock (evalE TBool) <$> checkCodeBlock cb

checkInstruction (Expr e) = do
    (pt, evalE) <- checkExpr e
    polyTypeCheck TVoid pt . pure $ TExpr (evalE TVoid)

checkExpr :: Expr -> Check (PolyType, Type -> TExpr)
checkExpr (BinOp OpPlus e1 e2)     = checkArOp OpPlus e1 e2
checkExpr (BinOp OpMinus e1 e2)    = checkArOp OpMinus e1 e2
checkExpr (BinOp OpProduct e1 e2)  = checkArOp OpProduct e1 e2
checkExpr (BinOp OpDivision e1 e2) = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    polyTypeCheck TFloat pt1 . polyTypeCheck TFloat pt2 $
        pure (toPolyType TFloat,
              TBinOp TFloat OpDivision <$> evalE1 <*> evalE2)

checkExpr (BinOp OpGt e1 e2)  = checkCompareOp OpGt e1 e2
checkExpr (BinOp OpGte e1 e2) = checkCompareOp OpGte e1 e2
checkExpr (BinOp OpLt e1 e2)  = checkCompareOp OpLt e1 e2
checkExpr (BinOp OpLte e1 e2) = checkCompareOp OpLte e1 e2

checkExpr (BinOp OpEq e1 e2)  = checkEqualOp OpEq e1 e2
checkExpr (BinOp OpNeq e1 e2) = checkEqualOp OpNeq e1 e2

checkExpr (BinOp OpAnd e1 e2) = checkLogOp OpAnd e1 e2
checkExpr (BinOp OpOr e1 e2)  = checkLogOp OpOr e1 e2

checkExpr (UnOp OpNegate e) = do
    (pt, evalE) <- checkExpr e
    t' <- resolvePolyTypes pt numericTypes
    pure (t', \t -> TUnOp t OpNot (evalE t))
checkExpr (UnOp OpNot e) = do
    (pt, evalE) <- checkExpr e
    polyTypeCheck TBool pt $
        pure (toPolyType TBool, const $ TUnOp TBool OpNot (evalE TBool))

checkExpr (FunApply name es) = do
    (FunType argts rt) <- lookupFunction name
    ts <- traverse checkExpr es
    checkArgs argts (map fst ts) $ pure (toPolyType rt,
      const . TFunApply rt name $ zipWith ($) (map snd ts) argts)
  where checkArgs argTs = foldl (.) id . zipWith polyTypeCheck argTs

checkExpr (Ident name) = do
    t <- lookupVar name
    pure (toPolyType t, const $ TIdent t name)

checkExpr (Value VoidValue)     = pure (toPolyType TVoid,
                                        const $ TValue TVoid TVoidValue)
checkExpr (Value (BoolValue v)) = pure (toPolyType TBool,
                                        const $ TValue TBool (TBoolValue v))
checkExpr (Value (RawValue s))  = pure (posTypes, castLiteral)
    where
        posTypes = filter (`isLiteralType` s) [TInt, TUint, TFloat]
        castLiteral TInt   = TValue TInt . TIntValue $ read s
        castLiteral TUint  = TValue TUint . TUintValue $ read s
        castLiteral TFloat = TValue TFloat . TFloatValue $ read s
        castLiteral t = error $ "impossible happened in casting literal "
                                ++ s ++ " to type " ++ show t

checkArOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr )
checkArOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    t'  <- resolvePolyTypes pt1 numericTypes
    t'' <- resolvePolyTypes t' pt2
    pure (t'', \t -> TBinOp t op (evalE1 t) (evalE2 t))

checkCompareOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkCompareOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    t'  <- resolvePolyTypes pt1 boolAndNumericTypes
    t'' <- resolvePolyTypes t' pt2
    let t = uniPolyType t''
    pure (toPolyType TBool, const $ TBinOp t op (evalE1 t) (evalE2 t))

checkEqualOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkEqualOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    t'  <- resolvePolyTypes pt1 boolAndNumericTypes
    t'' <- resolvePolyTypes t' pt2
    let t = uniPolyType t''
    pure (toPolyType TBool, const $ TBinOp t op (evalE1 t) (evalE2 t))

checkLogOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkLogOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    polyTypeCheck TBool pt1 . polyTypeCheck TBool pt2 $
        pure (toPolyType TBool,
              const $ TBinOp TBool op (evalE1 TBool) (evalE2 TBool))

-- | Returns the intersection between polytypes if it exists or
-- | throws error
resolvePolyTypes :: PolyType -> PolyType -> Check PolyType
resolvePolyTypes t1 t2 = when (null t) (throwError err) >> pure t
  where t   = t1 `intersect` t2
        err = UnresolvablePolyTypes t1 t2

-- | Selects the broadest type from a polytype
uniPolyType :: PolyType -> Type
uniPolyType pt = fromMaybe TVoid . msum $ map f ts where
    f t = if t `elem` pt then Just t else Nothing
    ts = [TBool, TUint, TInt, TFloat]

isLiteralType :: Type -> String -> Bool
isLiteralType TVoid _ = False
isLiteralType TBool _ = False
isLiteralType TInt s = case reads s :: [(Int, String)] of
                             [(_, "")] -> True
                             _         -> False
isLiteralType TUint s = case reads s :: [(Word64, String)] of
                              [(_, "")] -> True
                              _ -> False
isLiteralType TFloat s = case reads s :: [(Double, String)] of
                               [(_, "")] -> True
                               _ -> False
