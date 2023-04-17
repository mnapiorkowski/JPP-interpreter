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

checkNoInit :: Pos -> Type -> Ident -> TM TEnv
checkNoInit pos t id = do
    (varEnv, funcEnv) <- ask
    -- if Map.member id varEnv
    --     then throwE $
    --         "variable " ++ printTree id ++ " is already defined"
    -- else do
    let varEnv' = Map.insert id t varEnv
    return (varEnv', funcEnv)

checkInit :: Pos -> Type -> Ident -> Expr -> TM TEnv
checkInit pos t id e = do
    (varEnv, funcEnv) <- ask
    -- if Map.member id varEnv
    --     then throwE $
    --         "variable " ++ printTree id ++ " is already defined"
    -- else do
    exprT <- typeofExpr e
    if exprT /= t
        then throwE pos $ 
            "in definition of " ++ printType t ++ " " ++ printTree id ++ 
            ":\nwrong type of expression:\n" ++ printTree e
    else do
        let varEnv' = Map.insert id t varEnv
        return (varEnv', funcEnv)

checkDecl :: Type -> Item -> TM TEnv
checkDecl t i = case i of
    NoInit pos id -> checkNoInit pos t id
    Init pos id e -> checkInit pos t id e

checkDecls :: Type -> [Item] -> TM TEnv
checkDecls _ [] = ask
checkDecls t (i:is) = do
    env' <- checkDecl t i
    local (const env') $ checkDecls t is

checkSDecl :: Pos -> TType -> [Item] -> TM TEnv
checkSDecl pos tt is = do
    let t = convTType tt
    if t == Void
        then throwE pos $
            "cannot declare void-type variable: " ++ printTree is
    else do
        env' <- checkDecls t is
        return env'

checkSAss :: Pos -> Ident -> Expr -> TM TEnv
checkSAss pos id e = do
    varT <- typeofVar pos id
    checkUnaryOp pos varT e
    ask

checkSIncrDecr :: Pos -> Ident -> TM TEnv
checkSIncrDecr pos id = do
    t <- typeofVar pos id
    if t /= Int
        then throwE pos $
            "increment or decrement operator applied to non-int-type variable "
            ++ printTree id
    else ask

checkIfExpr :: Pos -> Expr -> TM ()
checkIfExpr pos e = do
    t <- typeofExpr e
    if t /= Bool
        then throwE pos $
            "expression in if statement is not bool-type: " ++ printTree e
    else return ()

checkElif :: Elif -> Bool -> TM TEnv
checkElif (SElif pos e b) isLoop = do
    checkIfExpr pos e
    checkBlock b isLoop

checkElifs :: [Elif] -> Bool -> TM TEnv
checkElifs [] _ = ask
checkElifs (e:es) isLoop = do
    env' <- checkElif e isLoop
    local (const env') $ checkElifs es isLoop

checkSIf :: Pos -> Expr -> Block -> [Elif] -> Bool -> TM TEnv
checkSIf pos e b elifs isLoop = do
    checkIfExpr pos e
    checkBlock b isLoop
    checkElifs elifs isLoop

checkSIfElse :: Pos -> Expr -> Block -> [Elif] -> Block -> Bool -> TM TEnv
checkSIfElse pos e bIf elifs bElse isLoop = do
    checkSIf pos e bIf elifs isLoop
    checkBlock bElse isLoop

checkSWhile :: Pos -> Expr -> Block -> TM TEnv
checkSWhile pos e b = do
    t <- typeofExpr e
    if t /= Bool
        then throwE pos $
            "expression in while statement is not bool-type: " ++ printTree e
    else checkBlock b True

checkSBreakContinue :: Pos -> Bool -> TM TEnv
checkSBreakContinue pos isLoop = do
    if not isLoop
        then throwE pos $
            "break or continue used outside of a loop"
    else ask

checkSPrint :: Pos -> Expr -> TM TEnv
checkSPrint pos e = do
    t <- typeofExpr e
    if t == Void
        then throwE pos $
            "tried to print void-type expression: " ++ printTree e
    else ask

checkStmt :: Stmt -> Bool -> TM TEnv -- TODO
checkStmt s isLoop = case s of
    SExpr _ e -> checkSExpr e
    SDecl pos t is -> checkSDecl pos t is
    SAss pos id e -> checkSAss pos id e
    SIncr pos id -> checkSIncrDecr pos id
    SDecr pos id -> checkSIncrDecr pos id
    SIf pos e b elifs -> checkSIf pos e b elifs isLoop
    SIfElse pos e bIf elifs bElse -> checkSIfElse pos e bIf elifs bElse isLoop
    SWhile pos e b -> checkSWhile pos e b
    SBreak pos -> checkSBreakContinue pos isLoop
    SContinue pos -> checkSBreakContinue pos isLoop
    SPrint pos e -> checkSPrint pos e

checkStmts :: [Stmt] -> Bool -> TM TEnv
checkStmts [] _ = ask
checkStmts (s:ss) isLoop = do
    env' <- checkStmt s isLoop
    local (const env') $ checkStmts ss isLoop

checkBlock :: Block -> Bool -> TM TEnv
checkBlock (BBlock _ ss) isLoop = checkStmts ss isLoop
