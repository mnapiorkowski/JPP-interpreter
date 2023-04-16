module Typechecker.Utils where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs

import Types

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

