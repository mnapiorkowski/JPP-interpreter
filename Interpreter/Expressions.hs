module Interpreter.Expressions where

import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types
import Utils

evalUnaryOp :: Expr -> IM Val
evalUnaryOp e = do
    v <- evalExpr e
    case v of
        IntV i -> return $ IntV (-i)
        BoolV b -> return $ BoolV (not b)

evalBinaryOpIB :: Expr -> (Int -> Int -> Bool) -> Expr -> IM Val
evalBinaryOpIB e1 op e2 = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    i1 <- case v1 of
        IntV i -> return i
    i2 <- case v2 of
        IntV i -> return i
    return $ BoolV (i1 `op` i2)

evalBinaryOpBB :: Expr -> (Bool -> Bool -> Bool) -> Expr -> IM Val
evalBinaryOpBB e1 op e2 = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    b1 <- case v1 of
        BoolV b -> return b
    b2 <- case v2 of
        BoolV b -> return b
    return $ BoolV (b1 `op` b2)

evalBinaryOpII :: Expr -> (Int -> Int -> Int) -> Expr -> IM Val
evalBinaryOpII e1 op e2 = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    i1 <- case v1 of
        IntV i -> return i
    i2 <- case v2 of
        IntV i -> return i
    return $ IntV (i1 `op` i2)

evalRelOp :: Expr -> RelOp -> Expr -> IM Val
evalRelOp e1 op e2 = case op of
    OLt _ -> evalBinaryOpIB e1 (<) e2
    OLeq _ -> evalBinaryOpIB e1 (<=) e2
    OGt _ -> evalBinaryOpIB e1 (>) e2
    OGeq _ -> evalBinaryOpIB e1 (>=) e2
    OEq _ -> evalBinaryOpIB e1 (==) e2
    ONeq _ -> evalBinaryOpIB e1 (/=) e2

evalLogicOp :: Expr -> (Bool -> Bool -> Bool) -> Expr -> IM Val
evalLogicOp e1 op e2 = evalBinaryOpBB e1 op e2

evalPlusOp :: Expr -> Expr -> IM Val
evalPlusOp e1 e2 = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case v1 of
        IntV i -> 
            case v2 of
                IntV i' -> return $ IntV (i + i')
        StringV s ->
            case v2 of
                StringV s' -> return $ StringV (s ++ s')

evalAddOp :: Expr -> AddOp -> Expr -> IM Val
evalAddOp e1 op e2 = case op of
    OPlus _ -> evalPlusOp e1 e2
    OMinus _ -> evalBinaryOpII e1 (-) e2

evalTimesOp :: Expr -> Expr -> IM Val
evalTimesOp e1 e2 = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case v1 of
        IntV i -> 
            case v2 of
                IntV i' -> return $ IntV (i * i')
                StringV s -> return $ StringV (concat $ replicate i s)
        StringV s ->
            case v2 of
                IntV i -> return $ StringV (concat $ replicate i s)

evalDivOp :: Pos -> Expr -> (Int -> Int -> Int) -> Expr -> IM Val
evalDivOp pos e1 op e2 = do
    v2 <- evalExpr e2
    case v2 of
        IntV 0 -> throwRuntimeE pos $ "cannot divide by zero"
        _ -> evalBinaryOpII e1 op e2

evalMulOp :: Expr -> MulOp -> Expr -> IM Val
evalMulOp e1 op e2 = case op of
    OTimes _ -> evalTimesOp e1 e2
    ODiv pos -> evalDivOp pos e1 (div) e2
    OMod pos -> evalDivOp pos e1 (mod) e2

evalVar :: Ident -> IM Val
evalVar id = do
    (varEnv, _) <- ask
    (store, newloc) <- get
    let loc = varEnv Map.! id
    let var = store Map.! loc
    case var of
        Evaled v -> return v
        NotEvaled e -> do
            v <- evalExpr e
            put $ (Map.insert loc (Evaled v) store, newloc)
            return v

evalArg :: Expr -> IM Arg
evalArg e = case e of
    EVar _ id -> do
        (varEnv, _) <- ask
        let loc = varEnv Map.! id
        return $ VarArg loc
    _ -> do
        v <- evalExpr e
        return $ ValArg v

evalArgs :: [Expr] -> IM [Arg]
evalArgs [] = return []
evalArgs (e:es) = do
    a <- evalArg e
    as <- evalArgs es
    return (a:as)

evalApp :: Ident -> [Expr] -> IM Val
evalApp id es = do
    (_, funcEnv) <- ask
    let Func f = funcEnv Map.! id
    args <- evalArgs es
    f args

evalExpr :: Expr -> IM Val
evalExpr e = case e of
    ELitInt _ i -> return $ IntV (fromInteger i)
    ELitTrue _ -> return $ BoolV True
    ELitFalse _ -> return $ BoolV False
    EString _ s -> return $ StringV s
    ENeg _ e -> evalUnaryOp e
    ENot _ e -> evalUnaryOp e
    ERel _ e1 op e2 -> evalRelOp e1 op e2
    EAnd _ e1 e2 -> evalLogicOp e1 (&&) e2
    EOr _ e1 e2 -> evalLogicOp e1 (||) e2
    EAdd _ e1 op e2 -> evalAddOp e1 op e2
    EMul _ e1 op e2 -> evalMulOp e1 op e2
    EVar _ id -> evalVar id
    EApp _ id es -> evalApp id es

evalExprs :: [Expr] -> IM [Val]
evalExprs [] = return []
evalExprs (e:es) = do
    v <- evalExpr e
    vs <- evalExprs es
    return (v:vs)
