module Typechecker where

import qualified Control.Monad.Except as E (throwError)
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs
import Grammar.Print (printTree)

import Types

checkUnaryOp :: Expr -> Type -> TM Type
checkUnaryOp e t = do
    exprT <- typeofExpr e
    if exprT /= t
        then throwE $
            "wrong type of expression: " ++ printTree e ++
            "\nexpected type: " ++ printTree t
    else return t

checkBinaryOp :: Type -> Expr -> Expr -> Type -> TM Type
checkBinaryOp expectedT e1 e2 retT = do
    exprT1 <- typeofExpr e1
    exprT2 <- typeofExpr e2
    if exprT1 /= expectedT
        then throwE $ 
            "wrong type of expression: " ++ printTree e1 ++
            "\nexpected type: " ++ printTree expectedT
    else if exprT2 /= expectedT
        then throwE $ 
            "wrong type of expression: " ++ printTree e2 ++
            "\nexpected type: " ++ printTree expectedT
    else return retT

checkVar :: Ident -> TM Type
checkVar id = do
    (varEnv, funcEnv) <- ask
    if Map.notMember id varEnv
        then throwE $
            "variable " ++ printTree id ++ " is not defined"
    else return $ varEnv Map.! id

checkApp :: Ident -> [Expr] -> TM Type
checkApp id es = do
    (varEnv, funcEnv) <- ask
    if Map.notMember id funcEnv
        then throwE $
            "function " ++ printTree id ++ " is not defined"
    else do
        let funcT = funcEnv Map.! id
        exprTs <- typeofExprs es
        if snd funcT /= exprTs
            then throwE $
                "arguments to function " ++ printTree id ++ 
                " do not match function's signature"
        else return $ fst funcT

typeofExpr :: Expr -> TM Type
typeofExpr e = case e of
    ELitInt i -> return TInt
    ELitTrue -> return TBool
    ELitFalse -> return TBool
    EString s -> return TString
    ENeg e -> checkUnaryOp e TInt
    ENot e -> checkUnaryOp e TBool
    EMul e1 op e2 -> checkBinaryOp TInt e1 e2 TInt
    EAdd e1 op e2 -> checkBinaryOp TInt e1 e2 TInt
    ERel e1 op e2 -> checkBinaryOp TInt e1 e2 TBool
    EAnd e1 e2 -> checkBinaryOp TBool e1 e2 TBool
    EOr e1 e2 -> checkBinaryOp TBool e1 e2 TBool
    EVar id -> checkVar id
    EApp id es -> checkApp id es

typeofExprs :: [Expr] -> TM [Type]
typeofExprs [] = return []
typeofExprs (e:es) = do
    t <- typeofExpr e
    ts <- typeofExprs es
    return (t:ts)

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

checkSExpr :: Expr -> TM TEnv
checkSExpr e = do
    _ <- typeofExpr e
    ask

checkSDecl :: Type -> [Item] -> TM TEnv
checkSDecl t is = do
    env' <- checkDecls t is
    return env'

checkSAss :: Ident -> Expr -> TM TEnv
checkSAss id e = do
    varT <- checkVar id
    exprT <- typeofExpr e
    if varT /= exprT
        then throwE $
            "wrong type of expression: " ++ printTree e ++
            "\nexpected type: " ++ printTree varT
    else ask

checkStmt :: Stmt -> TM TEnv -- TODO
checkStmt s = case s of
    SExpr e -> checkSExpr e
    SDecl t is -> checkSDecl t is
    SAss id e -> checkSAss id e
    SIncr id -> ask
    SDecr id -> ask
    SIf e b elifs -> ask
    SIfElse e bIf elifs bElse -> ask
    SWhile e b -> ask
    SBreak -> ask
    SContinue -> ask
    SPrint e -> ask

checkStmts :: [Stmt] -> TM TEnv
checkStmts [] = ask
checkStmts (s:ss) = do
    env' <- checkStmt s
    local (const env') $ checkStmts ss

checkBlock :: Block -> TM TEnv
checkBlock (BBlock ss) = checkStmts ss

checkFnDef :: Type -> Ident -> [Arg] -> Block -> Ret -> TM TEnv
checkFnDef t id as b r = do
    (varEnv, funcEnv) <- ask
    if Map.member id funcEnv
        then throwE $
            "function " ++ printTree id ++ " is already defined"
    else do
        argTs <- typeofArgs as
        env' <- checkArgs as
        env'' <- local (const env') $ checkBlock b
        -- retT <- typeofRet r
        -- if retT /= t
        --     then throwE $
        --         "return type of function " ++ printTree id ++
        --         " does not match function's signature"
        -- else do 
        let funcEnv' = Map.insert id (t, argTs) funcEnv
        return (varEnv, funcEnv') 

checkNoInit :: Type -> Ident -> TM TEnv -- ok
checkNoInit t id = do
    (varEnv, funcEnv) <- ask
    -- if Map.member id varEnv
    --     then throwE $
    --         "variable " ++ printTree id ++ " is already defined"
    -- else do
    let varEnv' = Map.insert id t varEnv
    return (varEnv', funcEnv)

checkInit :: Type -> Ident -> Expr -> TM TEnv
checkInit t id e = do
    (varEnv, funcEnv) <- ask
    -- if Map.member id varEnv
    --     then throwE $
    --         "variable " ++ printTree id ++ " is already defined"
    -- else do
    exprT <- typeofExpr e
    if exprT /= t
        then throwE $ 
            "in definition of " ++ printTree t ++ " " ++ printTree id ++ 
            ":\nwrong type of expression:\n" ++ printTree e
    else do
        let varEnv' = Map.insert id t varEnv
        return (varEnv', funcEnv)

checkDecl :: Type -> Item -> TM TEnv -- ok
checkDecl t i = case i of
    NoInit id -> checkNoInit t id
    Init id e -> checkInit t id e

checkDecls :: Type -> [Item] -> TM TEnv -- ok
checkDecls _ [] = ask
checkDecls t (i:is) = do
    env' <- checkDecl t i
    local (const env') $ checkDecls t is

checkTopDef :: TopDef -> TM TEnv -- ok
checkTopDef d = case d of
    FnDef t id as b r -> checkFnDef t id as b r
    GlobVar t is -> checkDecls t is

checkTopDefs :: [TopDef] -> TM TEnv -- ok
checkTopDefs [] = ask
checkTopDefs (d:ds) = do
    env' <- checkTopDef d
    local (const env') $ checkTopDefs ds

checkProgr :: Progr -> TM ()
checkProgr (Program ds) = do
    (varEnv, funcEnv) <- checkTopDefs ds
    -- throwE (listIdents (varEnv, funcEnv))
    let main = Ident "main"
    if Map.notMember main funcEnv
        then throwE "main function is not defined"
    else do
        let mainT = funcEnv Map.! main
        if fst mainT /= TVoid
            then throwE "main function is not void"
        else if snd mainT /= []
            then throwE "main function has arguments"
        else return ()

typecheck :: Progr -> Result () -- ok
typecheck p = do
    let initEnv = (Map.empty, Map.empty)
    runReaderT (checkProgr p) initEnv
    return ()



-- ****************** UTILS *****************

throwE :: String -> TM a
throwE s = lift $ E.throwError s

idToStr :: [Ident] -> [String]
idToStr [] = []
idToStr ((Ident s):is) = (s:(idToStr is))


-- lift $ throwE (listIdents env)
listIdents :: TEnv -> String
listIdents (varEnv, funcEnv) = "variables: " ++ vars ++ "\nfunctions: " ++ funcs
    where 
        vars = unwords $ idToStr (Map.keys varEnv)
        funcs = unwords $ idToStr (Map.keys funcEnv)

