module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs (Ident, BNFC'Position)

type Pos = BNFC'Position

type Result = Except String

data Type = IntT | StringT | BoolT | VoidT
    deriving (Eq) 

type TVarEnv = Map Ident Type
type TFuncEnv = Map Ident (Type, [Type])
type TEnv = (TVarEnv, TFuncEnv)

type TM a = ReaderT TEnv Result a -- typechecker monad

------

data Val = IntV Int | StringV String | BoolV Bool | VoidV

newtype Func = Func ([Val] -> IM Val)

type Loc = Int

type IVarEnv = Map Ident Loc
type IFuncEnv = Map Ident Func
type IEnv = (IVarEnv, IFuncEnv)

type IStore = Map Loc Val

type IM a = StateT IStore (ReaderT IEnv Result) a -- interpreter monad