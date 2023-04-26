module Utils where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types

throwE :: Pos -> String -> TM a
throwE pos s = lift $ E.throwError (posStr pos ++ s)

throwRuntimeE :: Pos -> String -> IM a
throwRuntimeE pos s = lift $ lift $ E.throwError (posStr pos ++ s)

posStr :: Pos -> String
posStr Nothing = "in unknown position:\n"
posStr (Just (l, c)) = "in line " ++ show l ++ ", column " ++ show c ++ ":\n"

printType :: Type -> String
printType IntT = "int"
printType BoolT = "bool"
printType StringT = "string"
printType VoidT = "void"

convTType :: TType -> Type
convTType (TInt _) = IntT
convTType (TBool _) = BoolT
convTType (TString _) = StringT
convTType (TVoid _) = VoidT

initVal :: TType -> Val
initVal (TInt _) = IntV 0
initVal (TBool _) = BoolV False
initVal (TString _) = StringV ""
initVal (TVoid _) = VoidV

idToStr :: [Ident] -> [String]
idToStr [] = []
idToStr ((Ident s):is) = (s:(idToStr is))


-- lift $ throwE (listIdents env)
listIdents :: TEnv -> String
listIdents (varEnv, funcEnv) = "variables: " ++ vars ++ "\nfunctions: " ++ funcs
    where 
        vars = unwords $ idToStr (Map.keys varEnv)
        funcs = unwords $ idToStr (Map.keys funcEnv)

listIdents' :: IEnv -> String
listIdents' (varEnv, funcEnv) = "variables: " ++ vars ++ "\nfunctions: " ++ funcs
    where 
        vars = unwords $ idToStr (Map.keys varEnv)
        funcs = unwords $ idToStr (Map.keys funcEnv)

