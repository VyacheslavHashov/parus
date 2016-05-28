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
               | NoReturn
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

numericTypes :: PolyType
numericTypes = [ TInt
               , TUint
               , TFloat]

boolAndNumericTypes :: PolyType
boolAndNumericTypes = [ TBool
                      , TInt
                      , TUint
                      , TFloat]

data CheckEnv = CheckEnv { ceGlobalVars :: Map.Map Name Type
                         , ceFunctions :: Map.Map Name FunType
                         , ceLocalVars :: Map.Map Name Type
                         , ceReturnType :: Type
                         }

type Check = ReaderT CheckEnv (Except TypeError)

typeCheck :: Type -> Type -> Check a -> Check a
typeCheck t1 t2 e = if t1 == t2
                        then e
                        else throwError $ TypeMismatch t1 t2

polyTypeCheck :: Type -> PolyType -> Check a -> Check a
polyTypeCheck t pt e = if t `elem` pt
                           then e
                           else throwError $ PolyTypeMismatch t pt

-- TODO move this function in another place
mkFunctionType :: Function -> FunType
mkFunctionType f = let rType    = fReturnType f
                       argTypes = map (fLocalVars f Map.!) $ fArgNames f
                    in FunType argTypes rType


typeAST :: AST -> Except TypeError TypedAST
typeAST ast = do
    let gVars  = globalVars ast
        fTypes = mkFunctionType <$> functions ast
    fs <- traverse (typeFunction gVars fTypes) $ functions ast
    pure TypedAST { tGlobalVars = gVars
                  , tFunctions = fs }

typeFunction :: Map.Map Name Type ->
                Map.Map Name FunType ->
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

    codeBlock <- runReaderT (typeFunCodeBlock $ fCodeBlock func)
                            checkEnv
    pure TFunction { tfType = currType
                   , tfArgNames = fArgNames func
                   , tfCodeBlock = codeBlock
                   }


-- Check here if return statement exists in codeblock
typeFunCodeBlock :: CodeBlock -> Check TCodeBlock
typeFunCodeBlock cb = do
    xs <- traverse (fmap f . checkInstruction) cb
    if True `elem` map snd xs
        then pure $ map fst xs
        else throwError NoReturn
  where f inst = (inst, isReturnStmt inst)

checkCodeBlock :: CodeBlock -> Check TCodeBlock
checkCodeBlock = traverse checkInstruction

checkInstruction :: Instruction -> Check TInstruction
checkInstruction (Assign name e) = do
    env <- ask
    case Map.lookup name (ceLocalVars env `Map.union` ceGlobalVars env) of
      Just t -> checkExpr e >>= \(pt, evalE) ->
                    polyTypeCheck t pt . pure $ TAssign name (evalE t)
      _      -> throwError $ NotInScopeVar name

checkInstruction (Return e) = do
    retType     <- ceReturnType <$> ask
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
        pure ([TFloat], \t -> TBinOp TFloat OpDivision (evalE1 t) (evalE2 t))

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
        pure ([TBool], const $ TUnOp TBool OpNot (evalE TBool))

checkExpr (FunApply name es) = do
    env <- ask
    case Map.lookup name $ ceFunctions env of
      Just (FunType argts rt) -> do
          ts <- traverse checkExpr es
          let checkArgs = foldl (.) id $
                  zipWith polyTypeCheck argts (map fst ts)
          checkArgs $ pure ([rt],
            const . TFunApply rt name $ zipWith ($) (map snd ts) argts)
      _ -> throwError $ NotInScopeFun name

checkExpr (Ident name) = do
    env <- ask
    case Map.lookup name $ ceLocalVars env `Map.union` ceGlobalVars env of
      Just t -> pure ([t], const $ TIdent t name)
      _      -> throwError $ NotInScopeVar name

checkExpr (Value VoidValue)     = pure ([TVoid], const $
                                       TValue TVoid TVoidValue)
checkExpr (Value (BoolValue v)) = pure ([TBool], const $
                                       TValue TBool (TBoolValue v))
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
    t'  <- resolvePolyTypes pt1 numericTypes
    t'' <- resolvePolyTypes t' pt2
    let t = uniPolyType t''
    pure ([TBool], const $ TBinOp t op (evalE1 t) (evalE2 t))

checkEqualOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkEqualOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    t'  <- resolvePolyTypes pt1 boolAndNumericTypes
    t'' <- resolvePolyTypes t' pt2
    let t = uniPolyType t''
    pure ([TBool], const $ TBinOp t op (evalE1 t) (evalE2 t))

checkLogOp :: BinOpType -> Expr -> Expr -> Check (PolyType, Type -> TExpr)
checkLogOp op e1 e2 = do
    (pt1, evalE1) <- checkExpr e1
    (pt2, evalE2) <- checkExpr e2
    polyTypeCheck TBool pt1 . polyTypeCheck TBool pt2 $
        pure ([TBool], const $ TBinOp TBool op (evalE1 TBool) (evalE2 TBool))

-- | Returns the intersection between polytypes if it exists or
-- | throws error
resolvePolyTypes :: PolyType -> PolyType -> Check PolyType
resolvePolyTypes t1 t2 = let t = t1 `intersect` t2
                        in case t of
                             [] -> throwError $ UnresolvablePolyTypes t1 t2
                             _  -> pure t

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
