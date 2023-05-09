module Typechecker.Program where

import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Types
import Utils

import Typechecker.Expressions ( typeofExpr )
import Typechecker.Statements ( checkBlock, checkSDecl, setVar )

typeofParam :: Param -> TM ParamT
typeofParam a = case a of
    ValParam _ tt _ -> return $ Type (convTType tt)
    RefParam _ tt _ -> return $ RefType (convTTypeRef tt)

typeofParams :: [Param] -> TM [ParamT]
typeofParams [] = return []
typeofParams (p:ps) = do
    t <- typeofParam p
    ts <- typeofParams ps
    return (t:ts)

typeofRet :: Ret -> TM Type
typeofRet r = case r of
    Return _ e -> typeofExpr e
    VReturn _ -> return VoidT
    Turnback _ e -> typeofExpr e
    VTurnback _ -> return VoidT

setFunc :: Type -> Ident -> [ParamT] -> TM TEnv
setFunc t id paramTs = do
    (varEnv, funcEnv) <- ask
    let funcEnv' = Map.insert id (t, paramTs) funcEnv
    return (varEnv, funcEnv') 

setFnDef :: Pos -> TType -> Ident -> [Param] -> TM TEnv
setFnDef pos tt id ps = do
    let t = convTType tt
    (_, funcEnv) <- ask
    if Map.member id funcEnv
        then throwE pos $
            "function " ++ printTree id ++ " is already defined"
    else do
        paramTs <- typeofParams ps
        setFunc t id paramTs

setGlobVar :: Pos -> Type -> Ident -> TM TEnv
setGlobVar pos t id = do
    (varEnv, _) <- ask
    if Map.member id varEnv
        then throwE pos $
            "global variable " ++ printTree id ++ " is already declared"
    else setVar t id

setGlobVars :: Pos -> Type -> [Item] -> TM TEnv
setGlobVars _ _ [] = ask
setGlobVars pos t (i:is) = do
    id <- case i of
        NoInit _ id' -> return id'
        Init _ id' _ -> return id'
    env' <- setGlobVar pos t id
    local (const env') $ setGlobVars pos t is

setTopDef :: TopDef -> TM TEnv
setTopDef d = case d of
    FnDef pos tt id as _ _ -> setFnDef pos tt id as
    GlobVar pos tt is -> do
        let t = convTType tt
        setGlobVars pos t is

setTopDefs :: [TopDef] -> TM TEnv
setTopDefs [] = ask
setTopDefs (d:ds) = do
    env' <- setTopDef d
    local (const env') $ setTopDefs ds

checkParam' :: Pos -> TType -> Ident -> TM TEnv
checkParam' pos tt id = do
    let t = convTType tt
    if t == VoidT
        then throwE pos $
            "parameter of a function cannot be void-type: " ++ printTree id
    else do
        (varEnv, _) <- ask
        if Map.member id varEnv
            then throwE pos $
                "parameters cannot have the same identifiers: " ++ printTree id
        else setVar t id

checkParam :: Param -> TM TEnv
checkParam p = case p of
    ValParam pos tt id -> checkParam' pos tt id
    RefParam pos tt id -> checkParam' pos tt id

checkParams :: [Param] -> TM TEnv
checkParams [] = ask
checkParams (p:ps) = do
    env' <- checkParam p
    local (const env') $ checkParams ps

mergeEnv :: TEnv -> TM TEnv
mergeEnv (varEnv, funcEnv) = do
    (oldVarEnv, oldFuncEnv) <- ask
    let newVarEnv = Map.union oldVarEnv varEnv
    let newFuncEnv = Map.union oldFuncEnv funcEnv
    return (newVarEnv, newFuncEnv)

checkFnDef :: Pos -> Ident -> [Param] -> Block -> Ret -> TM ()
checkFnDef pos id ps b r = do
    (varEnv, funcEnv) <- ask
    let (t, paramTs) = funcEnv Map.! id
    env1 <- local (const (Map.empty, Map.empty)) $ checkParams ps
    env2 <- local (const env1) $ setFunc t id paramTs -- recursion
    env3 <- local (const (varEnv, funcEnv)) $ mergeEnv env2
    env4 <- local (const env3) $ checkBlock b False
    env5 <- case r of
        Turnback _ _ -> local (const env4) $ checkBlock (reverseBlock b) False
        VTurnback _ -> local (const env4) $ checkBlock (reverseBlock b) False
        _ -> return env4
    retT <- local (const env5) $ typeofRet r
    if retT /= t
        then throwE pos $
            "return type of function " ++ printTree id ++
            " does not match function's signature"
    else return ()

checkGlobVar :: Pos -> TType -> [Item] -> TM ()
checkGlobVar pos tt is = do
    checkSDecl pos tt is
    return ()

checkTopDef :: TopDef -> TM ()
checkTopDef d = case d of
    FnDef pos tt id as b r -> checkFnDef pos id as b r
    GlobVar pos tt is -> checkGlobVar pos tt is

checkTopDefs :: [TopDef] -> TM ()
checkTopDefs [] = return ()
checkTopDefs (d:ds) = do
    checkTopDef d
    checkTopDefs ds

checkProgr :: Progr -> TM ()
checkProgr (Program pos ds) = do
    (varEnv, funcEnv) <- setTopDefs ds
    local (const (varEnv, funcEnv)) $ checkTopDefs ds
    let main = Ident "main"
    if Map.notMember main funcEnv
        then throwE pos $
            "main function is not defined"
    else do
        let (retT, paramTs) = funcEnv Map.! main
        if retT /= VoidT
            then throwE pos $
                "main function is not void-type"
        else if not $ null paramTs
            then throwE pos $
                "main function has more than zero parameters"
        else return ()

typecheck :: Progr -> Result ()
typecheck p = do
    let initEnv = (Map.empty, Map.empty)
    runReaderT (checkProgr p) initEnv
    return ()
