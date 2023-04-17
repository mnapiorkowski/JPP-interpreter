module Typechecker.Utils where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types

throwE :: Pos -> String -> TM a
throwE pos s = lift $ E.throwError (posStr pos ++ s)

posStr :: Pos -> String
posStr Nothing = "in unknown position:\n"
posStr (Just (l, c)) = "in line " ++ show l ++ ", column " ++ show c ++ ":\n"

printType :: Type -> String
printType Int = "int"
printType Bool = "bool"
printType String = "string"
printType Void = "void"

convTType :: TType -> Type
convTType (TInt _) = Int
convTType (TBool _) = Bool
convTType (TString _) = String
convTType (TVoid _) = Void

idToStr :: [Ident] -> [String]
idToStr [] = []
idToStr ((Ident s):is) = (s:(idToStr is))


-- lift $ throwE (listIdents env)
listIdents :: TEnv -> String
listIdents (varEnv, funcEnv) = "variables: " ++ vars ++ "\nfunctions: " ++ funcs
    where 
        vars = unwords $ idToStr (Map.keys varEnv)
        funcs = unwords $ idToStr (Map.keys funcEnv)

