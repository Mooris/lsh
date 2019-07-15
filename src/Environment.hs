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

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ atomicModifyIORef' envRef (\env -> (Map.insert var value env, value))

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
     where extendEnv env = return . Map.union env $ Map.fromList bindings
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

