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
    (varEnv, funcEnv, ctrl) <- ask
    let varEnv' = Map.insert id loc varEnv
    return (varEnv', funcEnv, ctrl)

setArg :: Param -> Arg -> IM IEnv
setArg p a = case p of 
    ValParam _ _ id -> case a of
        ValArg v -> newVar id (Evaled v)
        VarArg loc -> do
            (store, _) <- get
            let var = store Map.! loc
            newVar id var
    RefParam _ _ id -> case a of
        VarArg loc -> newVarRef id loc

setArgs :: [Param] -> [Arg] -> IM IEnv
setArgs [] [] = ask
setArgs (p:ps) (a:as) = do
    env' <- setArg p a
    local (const env') $ setArgs ps as

newFunc :: Ident -> [Param] -> Block -> Ret -> IM Func
newFunc id ps b r = do
    let f args = do
        env <- ask
        env1 <- local (const env) $ setArgs ps args
        env2 <- local (const env1) $ setFunc id (Func f)  -- recursion
        local (const env2) $ execFunc b r
    return $ Func f

setFunc :: Ident -> Func -> IM IEnv
setFunc id f = do
    (varEnv, funcEnv, ctrl) <- ask
    let funcEnv' = Map.insert id f funcEnv
    return (varEnv, funcEnv', ctrl)

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
    env@(_, funcEnv, _) <- setTopDefs ds
    let Func main = funcEnv Map.! (Ident "main")
    local (const env) $ main []
    return ()

interpret :: Progr -> Result ()
interpret p = do
    let initEnv = (Map.empty, Map.empty, Nothing)
    let initStore = (Map.empty, 0)
    runReaderT (runStateT (execProgr p) initStore) initEnv
    return ()
