module Typechecker.Expressions where

import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Typechecker.Utils

import Types

checkUnaryOp :: Pos -> Type -> Expr -> TM ()
checkUnaryOp pos t e = do
    exprT <- typeofExpr e
    if exprT /= t
        then throwE pos $
            "wrong type of expression: " ++ printTree e ++
            "\nexpected type: " ++ printType t
    else return ()

checkBinaryOp :: Pos -> Type -> Expr -> Expr -> TM ()
checkBinaryOp pos t e1 e2 = do
    checkUnaryOp pos t e1
    checkUnaryOp pos t e2

typeofAddOp :: Pos -> Expr -> AddOp -> Expr -> TM Type
typeofAddOp pos e1 op e2 = case op of
        OMinus pos -> checkBinaryOp pos Int e1 e2 >> return Int
        OPlus _ -> do
            exprT1 <- typeofExpr e1
            exprT2 <- typeofExpr e2
            if (exprT1 == Int && exprT2 == Int)
                then return Int
            else if (exprT1 == String && exprT2 == String)
                then return String
            else throwE pos $
                "operator '+' can be applied only on two ints or two strings"

typeofMulOp :: Pos -> Expr -> MulOp -> Expr -> TM Type
typeofMulOp pos e1 op e2 = case op of
        ODiv pos -> checkBinaryOp pos Int e1 e2 >> return Int
        OMod pos -> checkBinaryOp pos Int e1 e2 >> return Int
        OTimes _ -> do
            exprT1 <- typeofExpr e1
            exprT2 <- typeofExpr e2
            if (exprT1 == Int && exprT2 == Int)
                then return Int
            else if (exprT1 == Int && exprT2 == String) ||
                    (exprT1 == String && exprT2 == Int)
                then return String
            else throwE pos $
                " operator '*' can be applied only on two ints or string and int"

typeofVar :: Pos -> Ident -> TM Type
typeofVar pos id = do
    (varEnv, funcEnv) <- ask
    if Map.notMember id varEnv
        then throwE pos $
            "variable " ++ printTree id ++ " is not defined"
    else return $ varEnv Map.! id

typeofApp :: Pos -> Ident -> [Expr] -> TM Type
typeofApp pos id es = do
    (varEnv, funcEnv) <- ask
    if Map.notMember id funcEnv
        then throwE pos $
            "function " ++ printTree id ++ " is not defined"
    else do
        let funcT = funcEnv Map.! id
        exprTs <- typeofExprs es
        if snd funcT /= exprTs
            then throwE pos $
                "arguments to function " ++ printTree id ++ 
                " do not match function's signature"
        else return $ fst funcT

typeofExpr :: Expr -> TM Type
typeofExpr e = case e of
    ELitInt _ _ -> return Int
    ELitTrue _ -> return Bool
    ELitFalse _ -> return Bool
    EString _ _ -> return String
    ENeg pos e -> checkUnaryOp pos Int e >> return Int
    ENot pos e -> checkUnaryOp pos Bool e >> return Bool
    ERel pos e1 op e2 -> checkBinaryOp pos Int e1 e2 >> return Bool
    EAnd pos e1 e2 -> checkBinaryOp pos Bool e1 e2 >> return Bool
    EOr pos e1 e2 -> checkBinaryOp pos Bool e1 e2 >> return Bool
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
