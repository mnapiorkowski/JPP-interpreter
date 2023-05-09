module Interpreter.Statements where

import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types
import Utils

import Interpreter.Expressions ( evalExpr, evalVar )

newVar :: Ident -> Var -> IM IEnv
newVar id var = do
    (store, newloc) <- get
    (varEnv, funcEnv, ctrl) <- ask
    let varEnv' = Map.insert id newloc varEnv
    put $ (Map.insert newloc var store, succ newloc)
    return (varEnv', funcEnv, ctrl)

updateVar :: Ident -> Var -> IM IEnv
updateVar id var = do
    (store, newloc) <- get
    env@(varEnv, _, _) <- ask
    let loc = varEnv Map.! id 
    put $ (Map.insert loc var store, newloc)
    return env

execSExpr :: Expr -> IM IEnv
execSExpr e = do
    evalExpr e
    ask

execDecl :: Val -> Item -> IM IEnv
execDecl v i = case i of
    NoInit _ id -> newVar id (Evaled v)
    Init _ id e -> do
        v' <- evalExpr e
        newVar id (Evaled v')

execDecls :: Val -> [Item] -> IM IEnv
execDecls _ [] = ask
execDecls v (i:is) = do
    env' <- execDecl v i
    local (const env') $ execDecls v is

execSDecl :: TType -> [Item] -> IM IEnv
execSDecl tt is = do
    let v = initVal tt
    execDecls v is

execSAss :: Ident -> Expr -> IM IEnv
execSAss id e = do
    v <- evalExpr e
    updateVar id (Evaled v)

execSIncrDecr :: Ident -> (Int -> Int -> Int) -> IM IEnv
execSIncrDecr id op = do
    v <- evalVar id
    v' <- case v of
        IntV i -> return $ IntV (i `op` 1)
    updateVar id (Evaled v')

execElifs :: [Elif] -> Block -> IM IEnv
execElifs [] bElse = execBlock bElse
execElifs ((SElif _ e b):elifs) bElse = execSIfElse e b elifs bElse

execSIf :: Expr -> Block -> [Elif] -> IM IEnv
execSIf e b elifs = execSIfElse e b elifs (BBlock Nothing [])

execSIfElse :: Expr -> Block -> [Elif] -> Block -> IM IEnv
execSIfElse e bIf elifs bElse = do
    v <- evalExpr e
    cond <- case v of
        BoolV cond -> return cond
    if cond
        then execBlock bIf
    else execElifs elifs bElse

execWhileStmts :: [Stmt] -> IM IEnv
execWhileStmts [] = ask
execWhileStmts (s:ss) = do
    env@(varEnv, funcEnv, ctrl) <- execStmt s
    case ctrl of
        Just Break -> return (varEnv, funcEnv, Just Break)
        Just Continue -> return (varEnv, funcEnv, Nothing)
        Nothing -> local (const env) $ execWhileStmts ss

execWhileBlock :: Block -> IM IEnv
execWhileBlock (BBlock _ ss) = execWhileStmts ss

execSWhile :: Expr -> Block -> IM IEnv
execSWhile e b = do
    v <- evalExpr e
    cond <- case v of
        BoolV cond -> return cond
    if cond then do
        (varEnv, funcEnv, ctrl) <- execWhileBlock b
        case ctrl of
            Just Break -> return (varEnv, funcEnv, Nothing)
            _ -> execSWhile e b
    else ask

execSBreak :: IM IEnv
execSBreak = do
    (varEnv, funcEnv, _) <- ask
    return (varEnv, funcEnv, Just Break)

execSContinue :: IM IEnv
execSContinue = do
    (varEnv, funcEnv, _) <- ask
    return (varEnv, funcEnv, Just Continue)

execSPrint :: Expr -> IM IEnv
execSPrint e = do
    v <- evalExpr e
    liftIO $ putStr $ showVal v
    ask

execSPrintLn :: Expr -> IM IEnv
execSPrintLn e = do
    v <- evalExpr e
    liftIO $ putStrLn $ showVal v
    ask

execStmt :: Stmt -> IM IEnv
execStmt s = case s of
    SExpr _ e -> execSExpr e
    SDecl _ t is -> execSDecl t is
    SAss _ id e -> execSAss id e
    SIncr _ id -> execSIncrDecr id (+)
    SDecr _ id -> execSIncrDecr id (-)
    SIf _ e b elifs -> execSIf e b elifs
    SIfElse _ e bIf elifs bElse -> execSIfElse e bIf elifs bElse
    SWhile _ e b -> execSWhile e b
    SBreak _ -> execSBreak
    SContinue _ -> execSContinue
    SPrint _ e -> execSPrint e
    SPrintLn _ e -> execSPrintLn e

execStmts :: [Stmt] -> IM IEnv
execStmts [] = ask
execStmts (s:ss) = do
    env' <- execStmt s
    local (const env') $ execStmts ss

execBlock :: Block -> IM IEnv
execBlock (BBlock _ ss) = execStmts ss