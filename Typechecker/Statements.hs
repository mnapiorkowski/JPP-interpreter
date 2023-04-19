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

setVar :: Type -> Ident -> TM TEnv
setVar t id = do
    (varEnv, funcEnv) <- ask
    let varEnv' = Map.insert id t varEnv
    return (varEnv', funcEnv) 

checkNoInit :: Pos -> Type -> Ident -> Bool -> TM TEnv
checkNoInit pos t id isGlobal = do
    (varEnv, funcEnv) <- ask
    if (isGlobal && Map.member id varEnv)
        then throwE pos $
            "global variable " ++ printTree id ++ " is already defined"
    else setVar t id

checkInit :: Pos -> Type -> Ident -> Expr -> Bool -> TM TEnv
checkInit pos t id e isGlobal = do
    checkNoInit pos t id isGlobal
    exprT <- typeofExpr e
    if exprT /= t
        then throwE pos $ 
            "in definition of " ++ printType t ++ " " ++ printTree id ++ 
            ":\nwrong type of expression:" ++ printTree e
    else setVar t id

checkDecl :: Type -> Item -> Bool -> TM TEnv
checkDecl t i isGlobal = case i of
    NoInit pos id -> checkNoInit pos t id isGlobal
    Init pos id e -> checkInit pos t id e isGlobal

checkDecls :: Type -> [Item] -> Bool -> TM TEnv
checkDecls _ [] _ = ask
checkDecls t (i:is) isGlobal = do
    env' <- checkDecl t i isGlobal
    local (const env') $ checkDecls t is isGlobal

checkSDecl :: Pos -> TType -> [Item] -> Bool -> TM TEnv
checkSDecl pos tt is isGlobal = do
    let t = convTType tt
    if t == Void
        then throwE pos $
            "cannot declare void-type variable: " ++ printTree is
    else do
        env' <- checkDecls t is isGlobal
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
    ask

checkSIfElse :: Pos -> Expr -> Block -> [Elif] -> Block -> Bool -> TM TEnv
checkSIfElse pos e bIf elifs bElse isLoop = do
    checkSIf pos e bIf elifs isLoop
    checkBlock bElse isLoop
    ask

checkSWhile :: Pos -> Expr -> Block -> TM TEnv
checkSWhile pos e b = do
    t <- typeofExpr e
    if t /= Bool
        then throwE pos $
            "expression in while statement is not bool-type: " ++ printTree e
    else do
        checkBlock b True
        ask

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

checkStmt :: Stmt -> Bool -> TM TEnv
checkStmt s isLoop = case s of
    SExpr _ e -> checkSExpr e
    SDecl pos t is -> checkSDecl pos t is False
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
