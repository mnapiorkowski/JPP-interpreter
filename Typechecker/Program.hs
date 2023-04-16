module Typechecker.Program where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print ( printTree )

import Typechecker.Utils
import Typechecker.Expressions ( typeofExpr )
import Typechecker.Statements ( checkBlock, checkSDecl )

import Types

typeofArg :: Arg -> TM Type
typeofArg a = case a of
    ValArg t id -> return t
    RefArg t id -> return t

typeofArgs :: [Arg] -> TM [Type]
typeofArgs [] = return []
typeofArgs (a:as) = do
    t <- typeofArg a
    ts <- typeofArgs as
    return (t:ts)

typeofRet :: Ret -> TM Type
typeofRet r = case r of
    Return e -> typeofExpr e
    VReturn -> return TVoid
    Turnback e -> typeofExpr e
    VTurnback -> return TVoid

checkArg' :: Type -> Ident -> TM TEnv
checkArg' t id = do
    if t == TVoid
        then throwE $
            "argument of a function cannot be void type: " ++ printTree id
    else do
        (varEnv, funcEnv) <- ask
        let varEnv' = Map.insert id t varEnv
        return (varEnv', funcEnv)

checkArg :: Arg -> TM TEnv
checkArg a = case a of
    ValArg t id -> checkArg' t id
    RefArg t id -> checkArg' t id

checkArgs :: [Arg] -> TM TEnv
checkArgs [] = ask
checkArgs (a:as) = do
    env' <- checkArg a
    local (const env') $ checkArgs as

checkFnDef :: Type -> Ident -> [Arg] -> Block -> Ret -> TM TEnv
checkFnDef t id as b r = do
    (varEnv, funcEnv) <- ask
    if Map.member id funcEnv
        then throwE $
            "function " ++ printTree id ++ " is already defined"
    else do
        env' <- checkArgs as
        env'' <- local (const env') $ checkBlock b False
        retT <- local (const env'') $ typeofRet r
        if retT /= t
            then throwE $
                "return type of function " ++ printTree id ++
                " does not match function's signature"
        else do 
            argTs <- typeofArgs as
            let funcEnv' = Map.insert id (t, argTs) funcEnv
            return (varEnv, funcEnv') 

checkTopDef :: TopDef -> TM TEnv
checkTopDef d = case d of
    FnDef t id as b r -> checkFnDef t id as b r
    GlobVar t is -> checkSDecl t is

checkTopDefs :: [TopDef] -> TM TEnv
checkTopDefs [] = ask
checkTopDefs (d:ds) = do
    env' <- checkTopDef d
    local (const env') $ checkTopDefs ds

checkProgr :: Progr -> TM ()
checkProgr (Program ds) = do
    (varEnv, funcEnv) <- checkTopDefs ds
    let main = Ident "main"
    if Map.notMember main funcEnv
        then throwE "main function is not defined"
    else do
        let mainT = funcEnv Map.! main
        if fst mainT /= TVoid
            then throwE "main function is not void type"
        else if snd mainT /= []
            then throwE "main function cannot have arguments"
        else return ()

typecheck :: Progr -> Result () -- ok
typecheck p = do
    let initEnv = (Map.empty, Map.empty)
    runReaderT (checkProgr p) initEnv
    return ()
