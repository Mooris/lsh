module Environment where

import LispVal
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

type Env = IORef (Map.Map String LispVal)

nullEnv :: IO Env
nullEnv = newIORef Map.empty

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . Map.lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  = liftIO (Map.lookup var <$> readIORef envRef) >>= \case
  Just anything -> return anything
  Nothing -> throwError $ UnboundVar var

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = liftIO $ atomicModifyIORef' envRef (\env -> (Map.insert var value env, value))

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
     where extendEnv env = return . Map.union env $ Map.fromList bindings

