module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs (Ident, Expr, BNFC'Position)

-- common

type Pos = BNFC'Position

type Result = ExceptT String IO

-- typechecker

data Type = IntT | StringT | BoolT | VoidT
    deriving Eq

data RefType = IntRef | StringRef | BoolRef

data ParamT = Type Type | RefType RefType

type InLoop = Bool
type TVarEnv = Map Ident Type
type TFuncEnv = Map Ident (Type, [ParamT])
type TEnv = (TVarEnv, TFuncEnv, InLoop)

type TM a = ReaderT TEnv Result a

-- interpreter

data Val = IntV Int | StringV String | BoolV Bool | VoidV

data Arg = ValArg Val | VarArg Loc

newtype Func = Func ([Arg] -> IM Val)

type Loc = Int

data Var = Evaled Val | NotEvaled Expr

data LoopCtrl = Break | Continue

type Ctrl = Maybe LoopCtrl
type IVarEnv = Map Ident Loc
type IFuncEnv = Map Ident Func
type IEnv = (IVarEnv, IFuncEnv, Ctrl)

type IStore = (Map Loc Var, Loc)

type IM a = StateT IStore (ReaderT IEnv Result) a