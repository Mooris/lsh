module LispVal
  ( module LispVal
  , module Control.Monad.Except
  )
where

import GHC.IO.Handle
import           Control.Monad.Except
import Data.IORef
import qualified Data.Map as Map

data Output = Free | InHandle | InString
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Port Handle
             | ShellString String
             | ErrorString String
             | NoValue
             | PrimitiveFunc ([LispVal] -> LispVal)
             | IOFunc ((Output, [LispVal]) -> IOThrowsError LispVal)
             | ShellFunc String
             | Syntax LispVal
             | Elipsis
             | PrimitiveMacroTransformer ([LispVal] -> LispVal)
             | MacroTransformer { param :: String, body :: [LispVal],
                                  closure :: IORef (Map.Map String LispVal) }
             | Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: IORef (Map.Map String LispVal) }

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | BadSpecialForm String LispVal
               | NotFunction LispVal [LispVal]
               | UnboundVar String
               | InvalidSyntax LispVal
               | Default String
               | Interpolation String

instance Show LispError where
  show = showError

instance Show LispVal where
  show = showVal

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (ShellString contents) = contents
showVal (ErrorString contents) = contents
showVal (Atom   name    ) = name
showVal (Number contents) = show contents
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList x xs) =
  "(" ++ unwordsList x ++ " . " ++ showVal xs ++ ")"
showVal NoValue           = ""
showVal (PrimitiveFunc _) = "#<procedure>"
showVal (Port _)   = "<IO port>"
showVal (ShellFunc path) = "#<Shell-procedure " ++ path ++ ">"
showVal (IOFunc _) = "#<procedure>"
showVal u@PrimitiveMacroTransformer{} = "#<procedure>"
showVal u@MacroTransformer{} = "#<procedure>"
showVal (Syntax v) = "#<syntax " ++ showVal v ++ ">"
showVal Elipsis = "#<...>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

showError :: LispError -> String
showError (Default message) = message
showError (UnboundVar    varname) = "Variable " ++ varname ++ " is not bound"
showError (BadSpecialForm message form   ) = message ++ ": " ++ show form
showError (NotFunction func ags) =
  "attempt to apply non-procedure: " ++ show func ++ if not $ null args
    then " with args: " ++ show args
    else ""
 where
  args = map showVal ags
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (InvalidSyntax syntax) = "Invalid Syntax: " ++ show syntax
showError (Interpolation str) = "Interpolation error: " ++ show str


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError LispVal -> IO LispVal
runIOThrows action = extractValue <$> runExceptT (trapError action)
 where
  trapError act = catchError act (return . ErrorString . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
