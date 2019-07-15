{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           System.Console.Haskeline
import System.Environment
import qualified Language.C.Inline as C
import Control.Monad.IO.Class

import LispVal
import Primitives
import Parser
import Eval
import Environment
import Process
import System.Process

C.verbatim "#define _XOPEN_SOURCE 600"
C.verbatim "#define __USE_BSD"

C.include "<stdio.h>"
C.include "<stdlib.h>"
C.include "<fcntl.h>"
C.include "bite.h"

prompt :: String
prompt = "lsh> $ "

subPrompt :: String
subPrompt = "λ> "

{-
initPty :: IO ()
initPty = [C.block| void {
      masterFd = posix_openpt(O_RDWR);
      if (masterFd < 0) return;
      int rc = grantpt(masterFd);
      if (rc != 0) return;
      rc = unlockpt(masterFd);
      if (rc != 0) return;
    }|]
-}

setEnvVar :: Env -> IO Env
setEnvVar local_env = List . map toScheme <$> getEnvironment >>= \lst -> bindVars local_env [("ENV", lst)]
 where
  toScheme :: (String, String) -> LispVal
  toScheme (key, value) = DottedList [String key] (String value)

primitiveBindings :: IO Env
primitiveBindings = nullEnv
                    >>= setEnvVar
                    >>= flip bindVars (map (mkFunc IOFunc) ioPrimitives
                        ++ map (mkFunc PrimitiveFunc) primitives)
                    >>= \newEnv ->
                      runIOThrows (load "std.scm" >>= mapM_ (eval newEnv Free) >> return NoValue)
                      >> return newEnv
  where mkFunc constructor (var, func) = (var, constructor func)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint local_env expr =  evalString local_env expr >>= outputString

outputString :: LispVal -> IO ()
outputString NoValue = return ()
outputString (ShellString str) = putStr str
outputString (ErrorString str) = putStrLn str
outputString other = print other

evalString :: Env -> String -> IO LispVal
evalString local_env expr =
  runIOThrows $ last <$> (liftThrows (readExpr expr) >>= mapM (eval local_env Free))


main :: IO ()
main = getArgs >>= \case
  [] -> (runInputT defaultSettings (liftIO primitiveBindings >>= loop))
  [filename] -> liftIO primitiveBindings >>= \local_env -> void $ runIOThrows (load filename >>= mapM_ (eval local_env Free) >> return NoValue)
  _ -> putStrLn "Nope"
 where
  loop :: Env -> InputT IO ()
  loop local_env = getInputLine prompt >>= \case
    Nothing    -> void $ outputStrLn "See ya, Motherfucker !"
    Just ('!':input) -> liftIO (createProcess (shell input) >>= waitProcess) >> loop local_env
    Just input -> liftIO (evalAndPrint local_env input) >> loop local_env
      --(((eval env) . readExpr) input)  >>= \case
      --NoValue -> return ()
      --ShellString str -> (putStr . show) str
      --other -> (putStrLn . show) other) >> loop
