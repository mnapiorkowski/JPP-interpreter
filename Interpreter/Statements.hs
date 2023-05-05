module Interpreter.Statements where

import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types
import Utils

import Interpreter.Expressions ( evalExpr, evalVar, setEvaledVar )

execSExpr :: Expr -> IM IEnv
execSExpr e = do
    evalExpr e
    ask

setVar :: Val -> Item -> IM IEnv
setVar v i = case i of
    NoInit _ id -> setEvaledVar id v
    Init _ id e -> do
        v' <- evalExpr e
        setEvaledVar id v'

setVars :: Val -> [Item] -> IM IEnv
setVars _ [] = ask
setVars v (i:is) = do
    env' <- setVar v i
    local (const env') $ setVars v is

execSDecl :: TType -> [Item] -> IM IEnv
execSDecl tt is = do
    let v = initVal tt
    setVars v is

execSAss :: Ident -> Expr -> IM IEnv
execSAss id e = do
    (varEnv, funcEnv) <- ask
    (store, loc') <- get
    v <- evalExpr e
    let loc = varEnv Map.! id
    put $ (Map.insert loc (Evaled v) store, loc')
    return (varEnv, funcEnv)

execSIncrDecr :: Ident -> (Int -> Int -> Int) -> IM IEnv
execSIncrDecr id op = do
    (varEnv, funcEnv) <- ask
    (store, loc') <- get
    v <- evalVar id
    v' <- case v of
        IntV i -> return $ IntV (i `op` 1)
    let loc = varEnv Map.! id
    put $ (Map.insert loc (Evaled v') store, loc')
    return (varEnv, funcEnv)

execSPrint :: Expr -> IM IEnv
execSPrint e = do
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
    SIf _ e b elifs -> ask
    SIfElse _ e bIf elifs bElse -> ask
    SWhile _ e b -> ask
    SBreak _ -> ask
    SContinue _ -> ask
    SPrint _ e -> execSPrint e

execStmts :: [Stmt] -> IM IEnv
execStmts [] = ask
execStmts (s:ss) = do
    env' <- execStmt s
    local (const env') $ execStmts ss

execBlock :: Block -> IM IEnv
execBlock (BBlock _ ss) = execStmts ss