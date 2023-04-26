module Interpreter.Program where

import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types
import Utils

execFunc :: Block -> Ret -> IM Val
execFunc b r = do
    return VoidV -- TODO

setArg :: Arg -> Val -> IM IEnv
setArg a v = case a of 
    ValArg _ _ id -> setVar id v
    RefArg _ _ id -> setVar id v

setArgs :: [Arg] -> [Val] -> IM IEnv
setArgs [] [] = ask
setArgs (a:as) (v:vs) = do
    env' <- setArg a v
    local (const env') $ setArgs as vs

newFunc :: Ident -> [Arg] -> Block -> Ret -> IM Func
newFunc id as b r = do
    env <- ask
    let f args = do
        env1 <- local (const env) $ setArgs as args
        env2 <- local (const env1) $ setFunc id (Func f)  -- recursion
        local (const env2) $ execFunc b r
    return $ Func f

setFunc :: Ident -> Func -> IM IEnv
setFunc id f = do
    (varEnv, funcEnv) <- ask
    let funcEnv' = Map.insert id f funcEnv
    return (varEnv, funcEnv')

setVar :: Ident -> Val -> IM IEnv
setVar id v = do
    (varEnv, funcEnv) <- ask
    let loc = Map.size varEnv
    let varEnv' = Map.insert id loc varEnv
    modify $ Map.insert loc v
    return (varEnv', funcEnv) 

setFnDef :: Ident -> [Arg] -> Block -> Ret -> IM IEnv
setFnDef id as b r = do
    f <- newFunc id as b r
    setFunc id f

setGlobVar :: Val -> Item -> IM IEnv
setGlobVar v i = case i of
    NoInit _ id -> setVar id v
    Init _ id e -> setVar id v -- TODO setVar id (evalExpr e)

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
    let initStore = Map.empty
    runReaderT (runStateT (execProgr p) initStore) initEnv
    return ()
