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

showType :: Type -> String
showType IntT = "int"
showType BoolT = "bool"
showType StringT = "string"
showType VoidT = "void"

showVal :: Val -> String
showVal (IntV i) = show i
showVal (BoolV False) = "false"
showVal (BoolV True) = "true"
showVal (StringV s) = s
showVal (VoidV) = ""

convTType :: TType -> Type
convTType (TInt _) = IntT
convTType (TBool _) = BoolT
convTType (TString _) = StringT
convTType (TVoid _) = VoidT

convTTypeRef :: TType -> RefType
convTTypeRef (TInt _) = IntRef
convTTypeRef (TBool _) = BoolRef
convTTypeRef (TString _) = StringRef

initVal :: TType -> Val
initVal (TInt _) = IntV 0
initVal (TBool _) = BoolV False
initVal (TString _) = StringV ""
initVal (TVoid _) = VoidV

eqTypeRef :: Type -> RefType -> Bool
eqTypeRef IntT IntRef = True
eqTypeRef BoolT BoolRef = True
eqTypeRef StringT StringRef = True
eqTypeRef _ _ = False

reverseBlock :: Block -> Block
reverseBlock (BBlock pos ss) = BBlock pos $ reverse ss
