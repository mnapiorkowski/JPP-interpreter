module Typechecker.Expressions where

import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Types
import Utils

checkUnaryOp :: Pos -> Type -> Expr -> TM ()
checkUnaryOp pos t e = do
    exprT <- typeofExpr e
    if exprT /= t
        then throwE pos $
            "wrong type of expression: " ++ printTree e ++
            "\nexpected type: " ++ showType t
    else return ()

checkBinaryOp :: Pos -> Type -> Expr -> Expr -> TM ()
checkBinaryOp pos t e1 e2 = do
    checkUnaryOp pos t e1
    checkUnaryOp pos t e2

typeofAddOp :: Pos -> Expr -> AddOp -> Expr -> TM Type
typeofAddOp pos e1 op e2 = case op of
        OMinus pos -> checkBinaryOp pos IntT e1 e2 >> return IntT
        OPlus _ -> do
            exprT1 <- typeofExpr e1
            exprT2 <- typeofExpr e2
            if (exprT1 == IntT && exprT2 == IntT)
                then return IntT
            else if (exprT1 == StringT && exprT2 == StringT)
                then return StringT
            else throwE pos $
                "operator '+' applied to types " ++ showType exprT1 ++
                " and " ++ showType exprT2

typeofMulOp :: Pos -> Expr -> MulOp -> Expr -> TM Type
typeofMulOp pos e1 op e2 = case op of
        ODiv pos -> checkBinaryOp pos IntT e1 e2 >> return IntT
        OMod pos -> checkBinaryOp pos IntT e1 e2 >> return IntT
        OTimes _ -> do
            exprT1 <- typeofExpr e1
            exprT2 <- typeofExpr e2
            if (exprT1 == IntT && exprT2 == IntT)
                then return IntT
            else if (exprT1 == IntT && exprT2 == StringT) ||
                    (exprT1 == StringT && exprT2 == IntT)
                then return StringT
            else throwE pos $
                "operator '*' applied to types " ++ showType exprT1 ++
                " and " ++ showType exprT2

typeofVar :: Pos -> Ident -> TM Type
typeofVar pos id = do
    (varEnv, funcEnv) <- ask
    if Map.notMember id varEnv
        then throwE pos $
            "variable " ++ printTree id ++ " is not defined"
    else return $ varEnv Map.! id

checkParamArg :: Pos -> Ident -> ParamT -> Expr -> TM ()
checkParamArg pos id p e = do
    argT <- typeofExpr e
    case p of
        RefType paramT -> case e of
            EVar _ id -> do
                if not $ eqTypeRef argT paramT
                    then throwE pos $
                        "arguments to function " ++ printTree id ++ 
                        " do not match function's signature"
                else return ()
            _ -> throwE pos $
                    "non-variable argument passed as reference to function " ++ 
                    printTree id
        Type paramT -> do
            if argT /= paramT
                then throwE pos $
                    "arguments to function " ++ printTree id ++ 
                    " do not match function's signature"
            else return ()

checkParamsArgs :: Pos -> Ident -> [ParamT] -> [Expr] -> TM ()
checkParamsArgs _ _ [] [] = return ()
checkParamsArgs pos id [] _ = throwE pos $
    "too many arguments to function " ++ printTree id
checkParamsArgs pos id _ [] = throwE pos $
    "too few arguments to function " ++ printTree id
checkParamsArgs pos id (p:ps) (e:es) = do
    checkParamArg pos id p e
    checkParamsArgs pos id ps es

typeofApp :: Pos -> Ident -> [Expr] -> TM Type
typeofApp pos id es = do
    (varEnv, funcEnv) <- ask
    if id == Ident "main"
        then throwE pos $
            "called main function"
    else if Map.notMember id funcEnv
        then throwE pos $
            "function " ++ printTree id ++ " is not defined"
    else do
        let (retT, paramTs) = funcEnv Map.! id
        checkParamsArgs pos id paramTs es
        return retT

typeofExpr :: Expr -> TM Type
typeofExpr e = case e of
    ELitInt _ _ -> return IntT
    ELitTrue _ -> return BoolT
    ELitFalse _ -> return BoolT
    EString _ _ -> return StringT
    ENeg pos e -> checkUnaryOp pos IntT e >> return IntT
    ENot pos e -> checkUnaryOp pos BoolT e >> return BoolT
    ERel pos e1 op e2 -> checkBinaryOp pos IntT e1 e2 >> return BoolT
    EAnd pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
    EOr pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
    EAdd pos e1 op e2 -> typeofAddOp pos e1 op e2
    EMul pos e1 op e2 -> typeofMulOp pos e1 op e2
    EVar pos id -> typeofVar pos id
    EApp pos id es -> typeofApp pos id es

typeofExprs :: [Expr] -> TM [Type]
typeofExprs [] = return []
typeofExprs (e:es) = do
    t <- typeofExpr e
    ts <- typeofExprs es
    return (t:ts)
