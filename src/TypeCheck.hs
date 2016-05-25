{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Word
import Data.Maybe
import qualified Data.Map as Map

import AST


data TypeError = TypeMismatch Type Type
               | PolyTypeMismatch Type PolyType
               | UnresolvablePolyTypes PolyType PolyType
               | NotInScopeFun Name
               | NotInScopeVar Name
    deriving Show

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

numericTypes :: PolyType
numericTypes = [ ValType TInt
               , ValType TUint
               , ValType TFloat]

boolAndNumericTypes :: PolyType
boolAndNumericTypes = [ ValType TBool
                      , ValType TInt
                      , ValType TUint
                      , ValType TFloat]

data CheckEnv = CheckEnv { ceGlobalVars :: Map.Map Name Type
                         , ceFunctions :: Map.Map Name Type
                         , ceLocalVars :: Map.Map Name Type
                         , ceCurrentFunction :: Type
                         }

type Check = ReaderT CheckEnv (Except TypeError)

typeCheck :: Type -> Type -> Check a -> Check a
typeCheck t1 t2  e = if t1 == t2
                        then e
                        else throwError $ TypeMismatch t1 t2

polyTypeCheck :: Type -> PolyType -> Check a -> Check a
polyTypeCheck t pt e = if t `elem` pt
                          then e
                          else throwError $ PolyTypeMismatch t pt

-- TODO move this function in another place
mkFunctionType :: Function -> Type
mkFunctionType f = let rType    = fReturnType f
                       argTypes = map (fLocalVars f Map.!) $ fArgNames f
                    in FunType argTypes rType

-- TODO this function too
typeToPrimType :: Type -> PrimType
typeToPrimType (ValType t)    = t
typeToPrimType (FunType _ _)  = error "error at typeToPrimType"
typeToPrimType (ReturnType _) = error "error at typeToPrimType"


typeAST :: AST -> Except TypeError TypedAST
typeAST ast = do
    let gVars  = ValType <$> globalVars ast
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
        currType   = mkFunctionType func
        checkEnv   = CheckEnv { ceGlobalVars  = gVars
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

checkCodeBlock :: CodeBlock -> Check TCodeBlock
checkCodeBlock = undefined

checkInstruction :: Instruction -> Check (Type, TInstruction)
checkInstruction (Assign name e) = do
    env <- ask
    case Map.lookup name (ceLocalVars env `Map.union` ceGlobalVars env) of
      Just t -> do
          (t', e') <- checkExpr e
          if t == t'
              then pure (ValType TVoid, TAssign name <$> e')
              else throwError $ TypeMismatch t t'

      _      -> throwError $ NotInScopeVar name

checkInstruction (Return e) = undefined $ checkExpr e

-- checkInstruction (IfElseBlock e cb1 cb2) = typeCheck conVoid $
--     TIfElseBlock <$> typeProp (checkExpr e) (ValType TBool)
--                  <*> typeProp (checkCodeBlock cb1) (ValType TVoid)
--                  <*> typeProp (checkCodeBlock cb2) (ValType TVoid)

-- checkInstruction (WhileBlock e cb) = typeCheck conVoid $
--     TWhileBlock <$> typeProp (checkExpr e) (ValType TBool)
--                 <*> typeProp (checkCodeBlock cb) (ValType TVoid)

checkInstruction (Expr e) = TExpr <$> checkExpr e

checkExpr :: Expr -> Check (PolyType, Type -> TExpr)
checkExpr (BinOp OpPlus e1 e2)     = checkArOp OpPlus e1 e2
checkExpr (BinOp OpMinus e1 e2)    = checkArOp OpMinus e1 e2
checkExpr (BinOp OpProduct e1 e2)  = checkArOp OpProduct e1 e2
checkExpr (BinOp OpDivision e1 e2) = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    polyTypeCheck (ValType TFloat) pt1 $
        polyTypeCheck (ValType TFloat) pt2 $
            pure ([ValType TFloat],
                \t -> TBinOp TFloat OpDivision (evalE1 t) (evalE2 t))

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
    pure (t', \t -> TUnOp (typeToPrimType t) OpNot (evalE t))
checkExpr (UnOp OpNot e) = do
    (pt, evalE) <- checkExpr e
    polyTypeCheck (ValType TBool) pt $
        pure ([ValType TBool],
              const $ TUnOp TBool OpNot (evalE (ValType TBool)))

checkExpr (FunApply name es) = do
    env <- ask
    case Map.lookup name $ ceFunctions env of
      Just (FunType argts rt) -> do
          ts <- traverse checkExpr es
          let checkArgs = foldl (.) id $
                zipWith polyTypeCheck (map ValType argts) (map fst ts)
          checkArgs $ pure ([ValType rt],
            const . TFunApply rt name $ zipWith ($) (map snd ts)
                                                    (map ValType argts))
      _ -> throwError $ NotInScopeFun name

checkExpr (Ident name) = do
    env <- ask
    case Map.lookup name $ ceLocalVars env `Map.union` ceGlobalVars env of
      Just t -> pure ([t], const $ TIdent (typeToPrimType t) name)
      _      -> throwError $ NotInScopeVar name

checkExpr (Value VoidValue)     = pure ([ValType TVoid], const $
                                       TValue TVoid TVoidValue)
checkExpr (Value (BoolValue v)) = pure ([ValType TBool], const $
                                       TValue TBool (TBoolValue v))
checkExpr (Value (RawValue s))  = pure (posTypes, castLiteral)
    where
        posTypes = map ValType $
            filter (flip isLiteralPrimType s) [TInt, TUint, TFloat]
        castLiteral (ValType TInt)   = TValue TInt . TIntValue $ read s
        castLiteral (ValType TUint)  = TValue TUint . TUintValue $ read s
        castLiteral (ValType TFloat) = TValue TFloat . TFloatValue $ read s
        castLiteral t = error $ "impossible happened in casting literal "
                                ++ s ++ " to type " ++ show t

checkArOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr )
checkArOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    t'  <- resolvePolyTypes pt1 numericTypes
    t'' <- resolvePolyTypes t' pt2
    pure (t'', \t -> TBinOp (typeToPrimType t) op (evalE1 t) (evalE2 t))

checkCompareOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkCompareOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    t'  <- resolvePolyTypes pt1 numericTypes
    t'' <- resolvePolyTypes t' pt2
    let t = uniPolyType t''
    pure ([ValType TBool],
          const $ TBinOp (typeToPrimType t) op (evalE1 t) (evalE2 t))

checkEqualOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkEqualOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    t'  <- resolvePolyTypes pt1 boolAndNumericTypes
    t'' <- resolvePolyTypes t' pt2
    let t = uniPolyType t''
    pure ([ValType TBool],
          const $ TBinOp (typeToPrimType t) op (evalE1 t) (evalE2 t))

checkLogOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkLogOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    polyTypeCheck (ValType TBool) pt1 $
        polyTypeCheck (ValType TBool) pt2 $
            pure ([ValType TBool], const $
                TBinOp TBool op (evalE1 (ValType TBool))
                                (evalE2 (ValType TBool)))

resolvePolyTypes :: PolyType -> PolyType -> Check PolyType
resolvePolyTypes t1 t2 = let t = t1 `intersect` t2
                        in case t of
                             [] -> throwError $ UnresolvablePolyTypes t1 t2
                             _  -> pure t

uniPolyType :: PolyType -> Type
uniPolyType pt = fromMaybe (ValType TVoid) . msum $ map f ts where
    f t = if t `elem` pt then Just t else Nothing
    ts = [ValType TBool, ValType TUint, ValType TInt, ValType TFloat]


isLiteralPrimType :: PrimType -> String -> Bool
isLiteralPrimType TVoid _ = False
isLiteralPrimType TBool _ = False
isLiteralPrimType TInt s = case reads s :: [(Int, String)] of
                             [(_, "")] -> True
                             _         -> False
isLiteralPrimType TUint s = case reads s :: [(Word64, String)] of
                              [(_, "")] -> True
                              _ -> False
isLiteralPrimType TFloat s = case reads s :: [(Double, String)] of
                               [(_, "")] -> True
                               _ -> False
