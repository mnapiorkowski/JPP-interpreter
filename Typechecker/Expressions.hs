module Typechecker.Expressions where

import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Typechecker.Utils

import Types

checkUnaryOp :: Type -> Expr -> TM ()
checkUnaryOp t e = do
    exprT <- typeofExpr e
    if exprT /= t
        then throwE $
            "wrong type of expression: " ++ printTree e ++
            "\nexpected type: " ++ printTree t
    else return ()

checkBinaryOp :: Type -> Expr -> Expr -> TM ()
checkBinaryOp t e1 e2 = do
    exprT1 <- typeofExpr e1
    exprT2 <- typeofExpr e2
    checkUnaryOp t e1
    checkUnaryOp t e2

typeofVar :: Ident -> TM Type
typeofVar id = do
    (varEnv, funcEnv) <- ask
    if Map.notMember id varEnv
        then throwE $
            "variable " ++ printTree id ++ " is not defined"
    else return $ varEnv Map.! id

typeofApp :: Ident -> [Expr] -> TM Type
typeofApp id es = do
    (varEnv, funcEnv) <- ask
    if Map.notMember id funcEnv
        then throwE $
            "function " ++ printTree id ++ " is not defined"
    else do
        let funcT = funcEnv Map.! id
        exprTs <- typeofExprs es
        if snd funcT /= exprTs
            then throwE $
                "arguments to function " ++ printTree id ++ 
                " do not match function's signature"
        else return $ fst funcT

typeofExpr :: Expr -> TM Type
typeofExpr e = case e of
    ELitInt i -> return TInt
    ELitTrue -> return TBool
    ELitFalse -> return TBool
    EString s -> return TString
    ENeg e -> checkUnaryOp TInt e >> return TInt
    ENot e -> checkUnaryOp TBool e >> return TBool
    EMul e1 op e2 -> checkBinaryOp TInt e1 e2 >> return TInt
    EAdd e1 op e2 -> checkBinaryOp TInt e1 e2 >> return TInt
    ERel e1 op e2 -> checkBinaryOp TInt e1 e2 >> return TBool
    EAnd e1 e2 -> checkBinaryOp TBool e1 e2 >> return TBool
    EOr e1 e2 -> checkBinaryOp TBool e1 e2 >> return TBool
    EVar id -> typeofVar id
    EApp id es -> typeofApp id es

typeofExprs :: [Expr] -> TM [Type]
typeofExprs [] = return []
typeofExprs (e:es) = do
    t <- typeofExpr e
    ts <- typeofExprs es
    return (t:ts)
