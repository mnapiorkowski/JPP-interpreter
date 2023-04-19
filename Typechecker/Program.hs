module Typechecker.Program where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Typechecker.Utils
import Typechecker.Expressions ( typeofExpr )
import Typechecker.Statements ( checkBlock, checkSDecl, setVar )

import Types

typeofArg :: Arg -> TM Type
typeofArg a = case a of
    ValArg _ tt _ -> return $ convTType tt
    RefArg _ tt _ -> return $ convTType tt

typeofArgs :: [Arg] -> TM [Type]
typeofArgs [] = return []
typeofArgs (a:as) = do
    t <- typeofArg a
    ts <- typeofArgs as
    return (t:ts)

typeofRet :: Ret -> TM Type
typeofRet r = case r of
    Return _ e -> typeofExpr e
    VReturn _ -> return Void
    Turnback _ e -> typeofExpr e
    VTurnback _ -> return Void

checkArg' :: Pos -> TType -> Ident -> TM TEnv
checkArg' pos tt id = do
    let t = convTType tt
    if t == Void
        then throwE pos $
            "argument of a function cannot be void-type: " ++ printTree id
    else do
        (varEnv, _) <- ask
        if Map.member id varEnv
            then throwE pos $
                "arguments cannot have the same identifiers: " ++ printTree id
        else setVar t id

checkArg :: Arg -> TM TEnv
checkArg a = case a of
    ValArg pos t id -> checkArg' pos t id
    RefArg pos t id -> checkArg' pos t id

checkArgs :: [Arg] -> TM TEnv
checkArgs [] = ask
checkArgs (a:as) = do
    env' <- checkArg a
    local (const env') $ checkArgs as

setFunc :: Type -> Ident -> [Type] -> TM TEnv
setFunc t id argTs = do
    (varEnv, funcEnv) <- ask
    let funcEnv' = Map.insert id (t, argTs) funcEnv
    return (varEnv, funcEnv') 

mergeEnv :: TEnv -> TM TEnv
mergeEnv (varEnv, funcEnv) = do
    (oldVarEnv, oldFuncEnv) <- ask
    let newVarEnv = Map.union oldVarEnv varEnv
    let newFuncEnv = Map.union oldFuncEnv funcEnv
    return (newVarEnv, newFuncEnv)

checkFnDef :: Pos -> TType -> Ident -> [Arg] -> Block -> Ret -> TM TEnv
checkFnDef pos tt id as b r = do
    let t = convTType tt
    (varEnv, funcEnv) <- ask
    if Map.member id funcEnv
        then throwE pos $
            "function " ++ printTree id ++ " is already defined"
    else do
        argTs <- typeofArgs as
        env1 <- local (const (Map.empty, Map.empty)) $ checkArgs as
        env2 <- local (const env1) $ setFunc t id argTs -- recursion
        env3 <- local (const (varEnv, funcEnv)) $ mergeEnv env2
        env4 <- local (const env3) $ checkBlock b False
        retT <- local (const env4) $ typeofRet r
        if retT /= t
            then throwE pos $
                "return type of function " ++ printTree id ++
                " does not match function's signature"
        else setFunc t id argTs

checkTopDef :: TopDef -> TM TEnv
checkTopDef d = case d of
    FnDef pos tt id as b r -> checkFnDef pos tt id as b r
    GlobVar pos tt is -> checkSDecl pos tt is True

checkTopDefs :: [TopDef] -> TM TEnv
checkTopDefs [] = ask
checkTopDefs (d:ds) = do
    env' <- checkTopDef d
    local (const env') $ checkTopDefs ds

checkProgr :: Progr -> TM ()
checkProgr (Program pos ds) = do
    (varEnv, funcEnv) <- checkTopDefs ds
    let main = Ident "main"
    if Map.notMember main funcEnv
        then throwE pos $
            "main function is not defined"
    else do
        let mainT = funcEnv Map.! main
        if fst mainT /= Void
            then throwE pos $
                "main function is not void-type"
        else if snd mainT /= []
            then throwE pos $
                "main function cannot have arguments"
        else return ()

typecheck :: Progr -> Result () -- ok
typecheck p = do
    let initEnv = (Map.empty, Map.empty)
    runReaderT (checkProgr p) initEnv
    return ()
