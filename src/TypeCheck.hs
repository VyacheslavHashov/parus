{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Word
import qualified Data.Map as Map

import AST


data TypeError = TypeMismatch Type Type
               | PolyTypeMismatch PolyType PolyType
               | NotInScopeFun Name
               | NotInScopeVar Name


class HasType a where
    getType :: a -> Type

class HasPrimType a where
    getPrimType :: a -> PrimType

instance HasType TInstruction where
    getType TAssign{}      = ValType TVoid
    getType (TReturn e)    = ReturnType $ getPrimType e
    getType TIfElseBlock{} = ValType TVoid
    getType TWhileBlock{}  = ValType TVoid
    getType (TExpr e)      = ValType $ getPrimType e

instance HasType TFunction where
    getType = tfType

instance HasPrimType a => HasType a where
    getType v = ValType $ getPrimType v

instance HasPrimType TExpr where
    getPrimType (TBinOp t _ _ _)  = t
    getPrimType (TUnOp t _ _)     = t
    getPrimType (TFunApply t _ _) = t
    getPrimType (TIdent t _)      = t
    getPrimType (TValue t _)      = t

instance HasPrimType TValue where
    getPrimType TVoidValue      = TVoid
    getPrimType (TBoolValue _)  = TBool
    getPrimType (TIntValue _)   = TInt
    getPrimType (TUintValue _)  = TUint
    getPrimType (TFloatValue _) = TFloat

data CheckEnv = CheckEnv { ceGlobalVars :: Map.Map Name Type
                         , ceFunctions :: Map.Map Name Type
                         , ceLocalVars :: Map.Map Name Type
                         , ceCurrentFunction :: Type
                         }

type Check = ReaderT CheckEnv (Except TypeError)

-- | Container that checks types before returning value
data TypePropagation a = TypePropagation { typeProp :: Type -> Check a }

typeCheck :: Type -> Check a -> TypePropagation a
typeCheck t1 e = TypePropagation $ \t2 ->
    if t1 == t2
       then e
       else throwError $ TypeMismatch t1 t2

mkFunctionType :: Function -> Type
mkFunctionType f = let rType    = fReturnType f
                       argTypes = map (fLocalVars f Map.!) $ fArgNames f
                    in FunType argTypes rType

typeAST :: AST -> Except TypeError TypedAST
typeAST ast = do
    let gVars = ValType <$> globalVars ast
        fTypes = mkFunctionType <$> functions ast
    fs <- traverse (typeFunction gVars fTypes) $ functions ast
    pure TypedAST { tGlobalVars = gVars
                  , tFunctions = fs }

typeFunction :: Map.Map Name Type ->
                Map.Map Name Type ->
                Function ->
                Except TypeError TFunction
typeFunction gVars fTypes func = do
    let localTypes = ValType <$> fLocalVars func
        currType = mkFunctionType func
        checkEnv = CheckEnv { ceGlobalVars = gVars
                            , ceFunctions = fTypes
                            , ceLocalVars = localTypes
                            , ceCurrentFunction = currType
                            }
        typeConstraint = ReturnType $ fReturnType func
    codeBlock <- runReaderT (checkCodeBlock typeConstraint $ fCodeBlock func)
                            checkEnv
    pure TFunction { tfType = currType
                   , tfArgNames = fArgNames func
                   , tfCodeBlock = codeBlock
                   }

checkCodeBlock :: CodeBlock -> TypePropagation TCodeBlock
checkCodeBlock = undefined

checkInstruction :: Instruction -> TypePropagation TInstruction
checkInstruction (Assign name e) = typeCheck (ValType TVoid) $ do
    env <- ask
    case Map.lookup name (ceLocalVars env `Map.union` ceGlobalVars env) of
      Just t -> do
          TAssign name <$> typeProp (checkExpr e) t
      _ -> throwError $ NotInScopeVar name

checkInstruction (Return e) = undefined $ checkExpr e

checkInstruction (IfElseBlock e cb1 cb2) = typeCheck (ValType TVoid) $
    TIfElseBlock <$> typeProp (checkExpr e) (ValType TBool)
                 <*> typeProp (checkCodeBlock cb1) (ValType TVoid)
                 <*> typeProp (checkCodeBlock cb2) (ValType TVoid)

checkInstruction (WhileBlock e cb) = typeCheck (ValType TVoid) $
    TWhileBlock <$> typeProp (checkExpr e) (ValType TBool)
                <*> typeProp (checkCodeBlock cb) (ValType TVoid)

checkInstruction (Expr e) = typeCheck (ValType TVoid) $
    TExpr <$> typeProp (checkExpr e) (ValType TVoid)

checkExpr :: Expr -> TypePropagation TExpr
checkExpr (BinOp op e1 e2) = BinOp op (checkExpr e1) (checkExpr e2)
checkExpr (UnOp op e) = UnOp op $ checkExpr e
checkExpr (FunApply name es) = FunApply name $ map checkExpr es
checkExpr (Ident n) = Ident n
checkExpr (Value v) = Value $ typeValue v

typeValue :: Value -> Value
typeValue (RawValue lit) = undefined
typeValue v = v



-- | This function does not check type constraints like numeric a =>
polyTypeOfExpr :: Expr -> Check PolyType
polyTypeOfExpr (BinOp OpPlus e1 e2)     = resolveBinOp e1 e2
polyTypeOfExpr (BinOp OpMinus e1 e2)    = resolveBinOp e1 e2
polyTypeOfExpr (BinOp OpProduct e1 e2)  = resolveBinOp e1 e2
polyTypeOfExpr (BinOp OpDivision e1 e2) = resolveBinOp e1 e2

polyTypeOfExpr (BinOp OpGt e1 e2)  = pure [ValType TBool]
polyTypeOfExpr (BinOp OpGte e1 e2) = pure [ValType TBool]
polyTypeOfExpr (BinOp OpLt e1 e2)  = pure [ValType TBool]
polyTypeOfExpr (BinOp OpLte e1 e2) = pure [ValType TBool]
polyTypeOfExpr (BinOp OpEq e1 e2)  = pure [ValType TBool]
polyTypeOfExpr (BinOp OpNeq e1 e2) = pure [ValType TBool]
polyTypeOfExpr (BinOp OpAnd e1 e2) = pure [ValType TBool]
polyTypeOfExpr (BinOp OpOr e1 e2)  = pure [ValType TBool]

polyTypeOfExpr (UnOp OpNegate e) = polyTypeOfExpr e
polyTypeOfExpr (UnOp OpNot e) = polyTypeOfExpr e

polyTypeOfExpr (FunApply name es) = do
    env <- ask
    case Map.lookup name (ceFunctions env) of
      Just (FunType _ t) -> pure [ValType t]
      Just _ -> error $ "impossible happened in polyTypeOfExpr at FunApply"
      Nothing -> throwError $ NotInScopeFun name
polyTypeOfExpr (Ident name) = do
    env <- ask
    case Map.lookup name (ceLocalVars env `Map.union` ceGlobalVars env) of
      Just t -> pure [t]
      Nothing -> throwError $ NotInScopeVar name

polyTypeOfExpr (Value VoidValue) = pure []
polyTypeOfExpr (Value (BoolValue v)) = pure [ValType TBool]
polyTypeOfExpr (Value (RawValue v)) =
    pure . map ValType . filter (flip isLiteralPrimType v) $
        [TInt, TUint, TFloat]

resolveBinOp :: Expr -> Expr -> Check PolyType
resolveBinOp e1 e2  = do
    t1 <- polyTypeOfExpr e1
    t2 <- polyTypeOfExpr e2
    resolvePolyType t1 t2

resolvePolyType :: PolyType -> PolyType -> Check PolyType
resolvePolyType t1 t2 = let t = t1 `intersect` t2
                        in case t of
                             [] -> throwError $ PolyTypeMismatch t1 t2
                             _ -> pure t

isLiteralPrimType :: PrimType -> String -> Bool
isLiteralPrimType TVoid s = False
isLiteralPrimType TBool s = False
isLiteralPrimType TInt s = case reads s :: [(Int, String)] of
                             [(_, "")] -> True
                             _         -> False
isLiteralPrimType TUint s = case reads s :: [(Word64, String)] of
                              [(_, "")] -> True
                              _ -> False
isLiteralPrimType TFloat s = case reads s :: [(Double, String)] of
                               [(_, "")] -> True
                               _ -> False





















