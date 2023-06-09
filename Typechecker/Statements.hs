module Typechecker.Statements where

import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Types
import Utils

import Typechecker.Expressions ( typeofExpr, typeofVar, checkUnaryOp )

checkSExpr :: Expr -> TM TEnv  
checkSExpr e = do
    typeofExpr e
    ask

setVar :: Type -> Ident -> TM TEnv
setVar t id = do
    (varEnv, funcEnv, inLoop) <- ask
    let varEnv' = Map.insert id t varEnv
    return (varEnv', funcEnv, inLoop) 

checkNoInit :: Pos -> Type -> Ident -> TM TEnv
checkNoInit pos t id = do
    setVar t id

checkInit :: Pos -> Type -> Ident -> Expr -> TM TEnv
checkInit pos t id e = do
    checkNoInit pos t id
    exprT <- typeofExpr e
    if exprT /= t
        then throwE pos $ 
            "in definition of '" ++ showType t ++ " " ++ printTree id ++ 
            "' wrong type of expression: " ++ printTree e
    else setVar t id

checkDecl :: Type -> Item -> TM TEnv
checkDecl t i = case i of
    NoInit pos id -> checkNoInit pos t id
    Init pos id e -> checkInit pos t id e

checkDecls :: Type -> [Item] -> TM TEnv
checkDecls _ [] = ask
checkDecls t (i:is) = do
    env <- checkDecl t i
    local (const env) $ checkDecls t is

checkSDecl :: Pos -> TType -> [Item] -> TM TEnv
checkSDecl pos tt is = do
    let t = convTType tt
    if t == VoidT
        then throwE pos $
            "declared void-type variable '" ++ printTree is ++ "'"
    else do
        env <- checkDecls t is
        return env

checkSAss :: Pos -> Ident -> Expr -> TM TEnv
checkSAss pos id e = do
    varT <- typeofVar pos id
    checkUnaryOp pos varT e
    ask

checkSIncrDecr :: Pos -> Ident -> TM TEnv
checkSIncrDecr pos id = do
    t <- typeofVar pos id
    if t /= IntT
        then throwE pos $
            "increment or decrement operator applied to non-int-type variable '"
            ++ printTree id ++ "'"
    else ask

checkIfExpr :: Pos -> Expr -> TM ()
checkIfExpr pos e = do
    t <- typeofExpr e
    if t /= BoolT
        then throwE pos $
            "condition in if statement is not bool-type: " ++ printTree e
    else return ()

checkElif :: Elif -> TM TEnv
checkElif (SElif pos e b) = do
    checkIfExpr pos e
    checkBlock b

checkElifs :: [Elif] -> TM TEnv
checkElifs [] = ask
checkElifs (e:es) = do
    env <- checkElif e
    local (const env) $ checkElifs es

checkSIf :: Pos -> Expr -> Block -> [Elif] -> TM TEnv
checkSIf pos e b elifs = do
    checkIfExpr pos e
    checkBlock b
    checkElifs elifs
    ask

checkSIfElse :: Pos -> Expr -> Block -> [Elif] -> Block -> TM TEnv
checkSIfElse pos e bIf elifs bElse = do
    checkSIf pos e bIf elifs
    checkBlock bElse
    ask

checkSWhile :: Pos -> Expr -> Block -> TM TEnv
checkSWhile pos e b = do
    t <- typeofExpr e
    if t /= BoolT
        then throwE pos $
            "condition in while statement is not bool-type: " ++ printTree e
    else do
        env@(varEnv, funcEnv, _) <- ask
        local (const (varEnv, funcEnv, True)) $ checkBlock b
        return env

checkSBreakContinue :: Pos -> TM TEnv
checkSBreakContinue pos = do
    (_, _, inLoop) <- ask
    if not inLoop
        then throwE pos $
            "break or continue used outside of a loop"
    else ask

checkSPrint :: Pos -> Expr -> TM TEnv
checkSPrint pos e = do
    t <- typeofExpr e
    if t == VoidT
        then throwE pos $
            "tried to print void-type expression: " ++ printTree e
    else ask

checkStmt :: Stmt -> TM TEnv
checkStmt s = case s of
    SExpr _ e -> checkSExpr e
    SDecl pos t is -> checkSDecl pos t is
    SAss pos id e -> checkSAss pos id e
    SIncr pos id -> checkSIncrDecr pos id
    SDecr pos id -> checkSIncrDecr pos id
    SIf pos e b elifs -> checkSIf pos e b elifs
    SIfElse pos e bIf elifs bElse -> checkSIfElse pos e bIf elifs bElse
    SWhile pos e b -> checkSWhile pos e b
    SBreak pos -> checkSBreakContinue pos
    SContinue pos -> checkSBreakContinue pos
    SPrint pos e -> checkSPrint pos e
    SPrintLn pos e -> checkSPrint pos e

checkStmts :: [Stmt] -> TM TEnv
checkStmts [] = ask
checkStmts (s:ss) = do
    env <- checkStmt s
    local (const env) $ checkStmts ss

checkBlock :: Block -> TM TEnv
checkBlock (BBlock _ ss) = checkStmts ss
