module Main where

import System.IO
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )

import Control.Monad.Except

import Grammar.Abs    ( Progr )
import Grammar.Layout ( resolveLayout )
import Grammar.Par    ( pProgr, myLexer )

import qualified Typechecker.Program as T ( typecheck )
import qualified Interpreter.Program as I ( interpret )

parse :: String -> Either String Progr
parse s = pProgr (resolveLayout False $ myLexer s)

typecheck :: Progr -> Either String ()
typecheck p = runExcept $ T.typecheck p

interpret :: Progr -> Either String ()
interpret p = runExcept $ I.interpret p

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    []         -> getContents
    (path:_)   -> readFile path
  putStrLn "Parsing..."
  case parse input of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right progr -> do
      putStrLn "OK"
      putStrLn "Typechecking..."
      case typecheck progr of
        Left err -> do
          hPutStrLn stderr $ "Type error " ++ err
          exitFailure
        Right _ -> do
          putStrLn "OK"
          putStrLn "Interpreting..."
          case interpret progr of
            Left err -> do
                hPutStrLn stderr $ "Runtime error " ++ err
                exitFailure
            Right _ -> do
              putStrLn "OK"
