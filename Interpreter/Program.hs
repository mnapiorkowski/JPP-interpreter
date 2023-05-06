module Interpreter.Program where

import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types
import Utils

import Interpreter.Expressions ( evalExpr )
import Interpreter.Statements ( execBlock, newVar )

execFunc :: Block -> Ret -> IM Val
execFunc b (Return _ e) = do
    env <- execBlock b
    local (const env) $ evalExpr e

execFunc b (VReturn _) = do
    execBlock b
    return VoidV

execFunc b (Turnback _ e) = do
    env <- execBlock b
    env' <- local (const env) $ execBlock (reverseBlock b)
    local (const env') $ evalExpr e

execFunc b (VTurnback _) = do
    env <- execBlock b
    local (const env) $ execBlock (reverseBlock b)
    return VoidV

newVarRef :: Ident -> Loc -> IM IEnv
newVarRef id loc = do
    (store, _) <- get
    (varEnv, funcEnv) <- ask
    let varEnv' = Map.insert id loc varEnv
    return (varEnv', funcEnv)

setArg :: Param -> Arg -> IM IEnv
setArg p a = case p of 
    ValParam _ _ id -> case a of
        ValArg v -> newVar id (Evaled v)
        VarArg loc -> do
            (store, _) <- get
            let var = store Map.! loc
            newVar id var
    RefParam _ _ id -> case a of
        ValArg v -> ask -- error, TODO: handle in Typechecker
        VarArg loc -> newVarRef id loc -- TODO

setArgs :: [Param] -> [Arg] -> IM IEnv
setArgs [] [] = ask
setArgs (p:ps) (a:as) = do
    env' <- setArg p a
    local (const env') $ setArgs ps as

newFunc :: Ident -> [Param] -> Block -> Ret -> IM Func
newFunc id ps b r = do
    env <- ask
    let f args = do
        env1 <- local (const env) $ setArgs ps args
        env2 <- local (const env1) $ setFunc id (Func f)  -- recursion
        local (const env2) $ execFunc b r
    return $ Func f

setFunc :: Ident -> Func -> IM IEnv
setFunc id f = do
    (varEnv, funcEnv) <- ask
    let funcEnv' = Map.insert id f funcEnv
    return (varEnv, funcEnv')

setFnDef :: Ident -> [Param] -> Block -> Ret -> IM IEnv
setFnDef id ps b r = do
    f <- newFunc id ps b r
    setFunc id f

setGlobVar :: Val -> Item -> IM IEnv
setGlobVar v i = case i of
    NoInit _ id -> newVar id (Evaled v)
    Init _ id e -> newVar id (NotEvaled e)

setGlobVars :: Val -> [Item] -> IM IEnv
setGlobVars _ [] = ask
setGlobVars v (i:is) = do
    env' <- setGlobVar v i
    local (const env') $ setGlobVars v is

setTopDef :: TopDef -> IM IEnv
setTopDef d = case d of
    FnDef _ _ id as b r -> setFnDef id as b r
    GlobVar _ tt is -> do
        let v = initVal tt
        setGlobVars v is

setTopDefs :: [TopDef] -> IM IEnv
setTopDefs [] = ask
setTopDefs (d:ds) = do
    env' <- setTopDef d
    local (const env') $ setTopDefs ds

execProgr :: Progr -> IM ()
execProgr (Program _ ds) = do
    (varEnv, funcEnv) <- setTopDefs ds
    let Func main = funcEnv Map.! (Ident "main")
    local (const (varEnv, funcEnv)) $ main []
    return ()

interpret :: Progr -> Result ()
interpret p = do
    let initEnv = (Map.empty, Map.empty)
    let initStore = (Map.empty, 0)
    runReaderT (runStateT (execProgr p) initStore) initEnv
    return ()
