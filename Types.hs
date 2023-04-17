module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Grammar.Abs (Ident, BNFC'Position)

type Pos = BNFC'Position

type Loc = Int
type Result = Except String

-- interpreter
data IVal = StringV String | IntV Int | BoolV Bool | VoidV
type IEnv = Map Ident Loc
type IStore = Map Loc IVal
type IM a = StateT IStore (ReaderT IEnv Result) a -- interpreter monad

-- typechecker
data Type = Int | String | Bool | Void
    deriving (Eq) 
type VarEnv = Map Ident Type
type FuncEnv = Map Ident (Type, [Type])
type TEnv = (VarEnv, FuncEnv)
type TM a = ReaderT TEnv Result a -- typechecker monad