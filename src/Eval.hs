module Eval (
    module Eval
) where


import           LispVal
import           Process
import           Environment
import           Parser
import           Expander
import           Debug.Trace
import           Data.List
import           Data.List.Split
import           System.Posix.Files
import           System.Directory
import           Data.Maybe

makeFunc
  :: Monad m =>
     Maybe String
     -> Env
     -> [LispVal]
     -> [LispVal]
     -> m LispVal
makeFunc varargs local_env prams bdy = return $ Func (map showVal prams) varargs bdy local_env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

getVarName :: Env -> String -> IOThrowsError (String, String)
getVarName local_env = gv ""
 where
  gv :: String -> String -> IOThrowsError (String, String)
  gv _ [] = throwError $ Interpolation "Missing `}`"
  gv acc ('}':rst) =
    flip (,) rst <$> (showVal <$> (liftThrows (last <$> readExpr acc) >>= eval local_env InString))
  gv acc (x:xs) = gv (acc ++ [x]) xs

insertVar :: Env -> String -> IOThrowsError LispVal
insertVar local_env str = String <$> iv "" str
 where
  iv res [] = return res
  iv acc ('#':'{':rest) = getVarName local_env rest >>= \(var, rst) -> iv (acc ++ var) rst
  iv acc (x:xs) = iv (acc ++ [x]) xs

eval :: Env -> Output -> LispVal -> IOThrowsError LispVal
eval _ _ val@(ShellString _                         ) = return val
eval local_env _ (String s                              )     = insertVar local_env s
eval _ _ val@(Number _                              ) = return val
eval _ _ val@(Bool   _                              ) = return val
eval local_env outpt val@(Atom   a  ) = expandIfNeeded local_env val >>= \case
  Just form -> eval local_env outpt form
  Nothing -> getVar local_env a >>= \case
    vl@(Atom _) -> eval local_env outpt vl
    other -> return other
eval local_env outpt (    List   [Atom "if", predi, conseq]) = eval local_env outpt predi >>= \case
  Bool False  -> return NoValue
  _           -> eval local_env outpt conseq
eval local_env outpt (    List   [Atom "if", predi, conseq, alt]) = eval local_env outpt predi >>= \case
  Bool False -> eval local_env outpt alt
  _          -> eval local_env outpt conseq
--eval local_env outpt (List [val@(List _)]           ) = eval local_env outpt val
eval local_env _ (List [Atom "define-syntax", Atom keyword, expr]) =
  defineSyntax local_env keyword expr >> return NoValue
eval _ _ (List [Atom "quote", val]           ) = return val
eval local_env _ (List [Atom "load", String filename]) =
  load filename >>= mapM_ (eval local_env Free) >> return NoValue
eval local_env _ (List [Atom "set!", Atom var, form]) =
  eval local_env InString form >>= setVar local_env var >> return NoValue
eval local_env c (List [Atom "define", Atom var, form@(List (Atom "lambda" : _))]) =
  eval local_env c form >>= defineVar local_env var
eval local_env _ (List [Atom "define", Atom var, form]) =
  defineVar local_env var form >> return NoValue
  --eval local_env handleCommand form >>= defineVar local_env var
eval local_env _ (List (Atom "define" : List (Atom var : prams) : bdy)) =
  makeNormalFunc local_env prams bdy >>= defineVar local_env var >> return NoValue
eval local_env _ (List (Atom "define" : DottedList (Atom var : prams) varargs : bdy)) =
  makeVarArgs varargs local_env prams bdy >>= defineVar local_env var >> return NoValue
eval local_env _ (List (Atom "lambda" : List prams : bdy)) =
  makeNormalFunc local_env prams bdy 
eval local_env _ (List (Atom "lambda" : DottedList prams varargs : bdy)) =
  makeVarArgs varargs local_env prams bdy
eval local_env _ (List (Atom "lambda" : varargs@(Atom _) : bdy)) =
  makeVarArgs varargs local_env [] bdy
--eval local_env outpt (List (Atom "begin" : exprs)) = last <$> mapM (eval local_env outpt) exprs
eval local_env _ (List (Atom "syntax-rules" : List literals : rules)) =
  return $ syntaxRules literals rules
eval local_env outpt l@(List (function : args)) = 
  expandIfNeeded local_env l >>= \case
    Just form -> eval local_env outpt form
    Nothing -> eval local_env InString function >>= \case
      val@(PrimitiveFunc _) -> evalFunc local_env outpt args val
      val@(IOFunc _) -> evalFunc local_env outpt args val
      val@(ShellFunc _) -> evalFunc local_env outpt args val
      val@Func{} -> evalFunc local_env outpt args val
      fn -> throwError $ NotFunction fn args
eval _ _ (List []) = return NoValue
eval _ _ val = trace ("|" ++ show val ++ "| <- wtf") return val

evalFunc :: Env -> Output -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalFunc local_env outpt args val@(ShellFunc _) =
  mapM evalArgs args >>= apply outpt val
 where --TODO: find better
    evalArgs (Atom ('$' : tmp)) = getVar local_env tmp >>= eval local_env outpt
    evalArgs v@(Atom _) = return v
    evalArgs v = eval local_env InString v
evalFunc local_env outpt args func =
  mapM (eval local_env InString) args >>= apply outpt func

checkShellCmd :: Env -> IOThrowsError LispVal -> IOThrowsError LispVal
checkShellCmd local_env action = catchError action handleAction
 where
  handleAction :: LispError -> IOThrowsError LispVal
  handleAction (UnboundVar fn) = checkPath local_env fn
  handleAction val = throwError val

apply :: Output -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply outpt (IOFunc func) args = func (outpt, args)
apply outpt (ShellFunc path) args = liftIO $ handleCommand outpt path args
apply _ (PrimitiveFunc func) args = return $ func args
apply _ (PrimitiveMacroTransformer func) args = return $ func args
apply outpt (MacroTransformer pram bdy closre) [arg] =
      liftIO (bindVars closre [(pram, Syntax arg)])
      >>= \le_env -> last <$> mapM (eval le_env outpt) bdy
apply outpt (Func prams varargs bdy closre) args =
      if num prams /= num args && isNothing varargs
      then throwError $ NumArgs (num prams) args
      else  liftIO (bindVars closre $ zip prams args)
            >>= bindVarArgs varargs
            >>= evalBody
      where remainingArgs = drop (length prams) args
            num = toInteger . length
            evalBody local_env = last <$> mapM (eval local_env outpt) bdy
            bindVarArgs arg local_env = case arg of
                Just argName -> liftIO $ bindVars local_env [(argName, List remainingArgs)]
                Nothing -> return local_env
apply outpt (Atom fn) ags = liftIO $ handleCommand outpt fn ags
apply _ fn ags = throwError $ NotFunction fn ags

extractPath :: Env -> IOThrowsError [String]
extractPath local_env = splitOn ":" <$> (getVar local_env "ENV"
                                        >>= \(List l) -> findPath l)
 where
  findPath :: [LispVal] -> IOThrowsError String
  findPath pth = case find (\(DottedList [String key] _) -> key == "PATH") pth of
      Just (DottedList _ (String value)) -> return value
      Nothing -> throwError $ UnboundVar "PATH"

isExecutable :: String -> IO Bool
isExecutable path = (==) ownerExecuteMode . intersectFileModes ownerExecuteMode . fileMode <$> getFileStatus path

defineSyntax :: Env -> String -> LispVal -> IOThrowsError LispVal
defineSyntax local_env kw expr = eval local_env InString expr >>= \case
  fun@Func{} -> defineVar local_env kw $ MacroTransformer ((head . params) fun) (body fun) (closure fun)
  fun@(PrimitiveFunc fn) -> defineVar local_env kw $ PrimitiveMacroTransformer fn

expandIfNeeded :: Env -> LispVal -> IOThrowsError (Maybe LispVal)
expandIfNeeded local_env val@(Atom a) = expandIfNeeded' local_env a val
expandIfNeeded local_env l@(List (Atom fn : _)) = expandIfNeeded' local_env fn l
expandIfNeeded _ _ = return Nothing --  trace (show val) (return (Just val))

expandIfNeeded' :: Env -> String -> LispVal -> IOThrowsError (Maybe LispVal)
expandIfNeeded' le name expr = catchError (Just <$> getVar le name) handleLookup
  >>= \case
    Just e@PrimitiveMacroTransformer{} -> transform e
    Just e@MacroTransformer{} -> transform e
    _ -> return Nothing
 where
  handleLookup :: LispError -> IOThrowsError (Maybe LispVal)
  handleLookup (UnboundVar _) = return Nothing
  handleLookup o = throwError o
  transform transformer = apply InString transformer [expr] >>= \case
    Syntax v -> return $ Just v
    _ -> throwError $ InvalidSyntax expr 

checkPath :: Env -> String -> IOThrowsError LispVal
--checkPath local_env (List (val@(Atom _) : xs)) =  List . flip (:) xs <$> checkPath local_env val
checkPath local_env str = extractPath local_env >>= liftIO . findBinaryPath >>= \case
  Nothing -> throwError $ UnboundVar str
  Just binaryPath -> (return . ShellFunc) binaryPath
 where
  findBinaryPath pths = findFileWith isExecutable pths str


