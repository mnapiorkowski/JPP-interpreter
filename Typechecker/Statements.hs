module Typechecker.Statements where

import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Typechecker.Utils
import Typechecker.Expressions ( typeofExpr, typeofVar, checkUnaryOp )

import Types

checkSExpr :: Expr -> TM TEnv  
checkSExpr e = do
    _ <- typeofExpr e
    ask

checkNoInit :: Type -> Ident -> TM TEnv
checkNoInit t id = do
    (varEnv, funcEnv) <- ask
    -- if Map.member id varEnv
    --     then throwE $
    --         "variable " ++ printTree id ++ " is already defined"
    -- else do
    let varEnv' = Map.insert id t varEnv
    return (varEnv', funcEnv)

checkInit :: Type -> Ident -> Expr -> TM TEnv
checkInit t id e = do
    (varEnv, funcEnv) <- ask
    -- if Map.member id varEnv
    --     then throwE $
    --         "variable " ++ printTree id ++ " is already defined"
    -- else do
    exprT <- typeofExpr e
    if exprT /= t
        then throwE $ 
            "in definition of " ++ printTree t ++ " " ++ printTree id ++ 
            ":\nwrong type of expression:\n" ++ printTree e
    else do
        let varEnv' = Map.insert id t varEnv
        return (varEnv', funcEnv)

checkDecl :: Type -> Item -> TM TEnv
checkDecl t i = case i of
    NoInit id -> checkNoInit t id
    Init id e -> checkInit t id e

checkDecls :: Type -> [Item] -> TM TEnv
checkDecls _ [] = ask
checkDecls t (i:is) = do
    env' <- checkDecl t i
    local (const env') $ checkDecls t is

checkSDecl :: Type -> [Item] -> TM TEnv
checkSDecl t is = do
    if t == TVoid
        then throwE $
            "cannot declare void-type variable: " ++ printTree is
    else do
        env' <- checkDecls t is
        return env'

checkSAss :: Ident -> Expr -> TM TEnv
checkSAss id e = do
    varT <- typeofVar id
    checkUnaryOp varT e
    ask

checkSIncrDecr :: Ident -> TM TEnv
checkSIncrDecr id = do
    t <- typeofVar id
    if t /= TInt
        then throwE $
            "increment or decrement operator applied to non-int-type variable "
            ++ printTree id
    else ask

checkIfExpr :: Expr -> TM ()
checkIfExpr e = do
    t <- typeofExpr e
    if t /= TBool
        then throwE $
            "expression in if statement is not bool-type: " ++ printTree e
    else return ()

checkElif :: Elif -> Bool -> TM TEnv
checkElif (SElif e b) isLoop = do
    checkIfExpr e
    checkBlock b isLoop

checkElifs :: [Elif] -> Bool -> TM TEnv
checkElifs [] _ = ask
checkElifs (e:es) isLoop = do
    env' <- checkElif e isLoop
    local (const env') $ checkElifs es isLoop

checkSIf :: Expr -> Block -> [Elif] -> Bool -> TM TEnv
checkSIf e b elifs isLoop = do
    checkIfExpr e
    checkBlock b isLoop
    checkElifs elifs isLoop

checkSIfElse :: Expr -> Block -> [Elif] -> Block -> Bool -> TM TEnv
checkSIfElse e bIf elifs bElse isLoop = do
    checkSIf e bIf elifs isLoop
    checkBlock bElse isLoop

checkSWhile :: Expr -> Block -> TM TEnv
checkSWhile e b = do
    t <- typeofExpr e
    if t /= TBool
        then throwE $
            "expression in while statement is not bool-type: " ++ printTree e
    else checkBlock b True

checkSBreakContinue :: Bool -> TM TEnv
checkSBreakContinue isLoop = do
    if not isLoop
        then throwE $
            "break or continue used outside of a loop"
    else ask

checkSPrint :: Expr -> TM TEnv
checkSPrint e = do
    t <- typeofExpr e
    if t == TVoid
        then throwE $
            "tried to print void-type expression: " ++ printTree e
    else ask

checkStmt :: Stmt -> Bool -> TM TEnv -- TODO
checkStmt s isLoop = case s of
    SExpr e -> checkSExpr e
    SDecl t is -> checkSDecl t is
    SAss id e -> checkSAss id e
    SIncr id -> checkSIncrDecr id
    SDecr id -> checkSIncrDecr id
    SIf e b elifs -> checkSIf e b elifs isLoop
    SIfElse e bIf elifs bElse -> checkSIfElse e bIf elifs bElse isLoop
    SWhile e b -> checkSWhile e b
    SBreak -> checkSBreakContinue isLoop
    SContinue -> checkSBreakContinue isLoop
    SPrint e -> checkSPrint e

checkStmts :: [Stmt] -> Bool -> TM TEnv
checkStmts [] _ = ask
checkStmts (s:ss) isLoop = do
    env' <- checkStmt s isLoop
    local (const env') $ checkStmts ss isLoop

checkBlock :: Block -> Bool -> TM TEnv
checkBlock (BBlock ss) isLoop = checkStmts ss isLoop
