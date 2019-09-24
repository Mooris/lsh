module Primitives where

import LispVal
import System.IO
import Eval
import Parser
import Process
import  System.Process
import System.Environment

fixNeg :: [LispVal] -> [LispVal]
fixNeg args@[_] = Number 0 : args
fixNeg a = a

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-) . fixNeg)
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("modulo"      , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

ioPrimitives :: [(String, (Output, [LispVal]) -> IOThrowsError LispVal)]
ioPrimitives =
  [("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("display", displayProc)
  , ("newline", newlineProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  , ("command-line", giveArgs)
  , ("|", handlePipe)
  ]


boolBinop :: (LispVal -> a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
boolBinop unpacker op [lhs, rhs] | left <- unpacker lhs
                                 , right <- unpacker rhs = Bool $ left `op` right
boolBinop _ _ _ = String "Wrong number of arguments"-- Wrong arg number TODO: Do something about it

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
numBoolBinop  = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> LispVal
strBoolBinop  = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed then 0 else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0

unpackStr :: LispVal -> String
unpackStr (String s) = s
unpackStr (Number s) = show s
unpackStr (Bool s)   = show s
unpackStr _  = "Invalid argument: not a string"

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool _  = False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op prams = Number $ evalArgs
 where
  evalArgs =
    foldl1 op $ map unpackNum prams

car :: [LispVal] -> LispVal
car [List (x : _)]         = x
car [DottedList (x : _) _] = x
car [_]                = Atom "car: invalid argument"
car _              = Atom "car: invalid argument"

cdr :: [LispVal] -> LispVal
cdr [List (_ : xs)]         = List xs
cdr [DottedList [_] x]      = x
cdr [DottedList (_ : xs) x] = DottedList xs x
cdr [_]                = Atom "cdr: Wtf bitch"
cdr _              = Atom "cdr: Wtf bitch"

cons :: [LispVal] -> LispVal
cons [x1, List []] = List [x1]
cons [x, List xs] = List $ x : xs
cons [x, DottedList xs xlast] = DottedList (x : xs) xlast
cons [x1, x2] = DottedList [x1] x2
cons _ = String "cons: Wtf m8"

eqv :: [LispVal] -> LispVal
eqv = eqv' . (map fixShellString)
 where
  fixShellString (ShellString str) = String str
  fixShellString val = val

eqv' :: [LispVal] -> LispVal
eqv' [(Bool arg1), (Bool arg2)]             = Bool $ arg1 == arg2
eqv' [(Number arg1), (Number arg2)]         = Bool $ arg1 == arg2
eqv' [(String arg1), (String arg2)]         = Bool $ arg1 == arg2
eqv' [(Atom arg1), (Atom arg2)]             = Bool $ arg1 == arg2
eqv' [(DottedList xs x), (DottedList ys y)] = eqv' [List $ xs ++ [x], List $ ys ++ [y]]
eqv' [(List arg1), (List arg2)]             = Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv' [x1, x2] of
                                            Bool bi -> bi
                                            _ -> False
eqv' [_, _]                                 = Bool False
eqv' badArgList                             = String ("Bad argument count in eqv: " ++ (show (length badArgList)))

equal :: [LispVal] -> LispVal
equal [lhs, rhs] = Bool $ (show lhs) == (show rhs)
equal _ = String $ "Bad argument count in equal"

applyProc :: (Output, [LispVal]) -> IOThrowsError LispVal
applyProc (outpt, [func, List args]) = apply outpt func args
applyProc (outpt, (func : args))     = apply outpt func args
applyProc _ = throwError $ Default "Invalid argument"

makePort :: IOMode -> (Output, [LispVal]) -> IOThrowsError LispVal
makePort mode (_, [String filename]) = liftM Port $ liftIO $ openFile filename mode
makePort _ _ = throwError $ Default "Invalid argument"

closePort :: (Output, [LispVal]) -> IOThrowsError LispVal
closePort (_, [Port port]) = liftIO $ hClose port >> (return $ Bool True)
closePort (_, _)           = return $ Bool False

readProc :: (Output, [LispVal]) -> IOThrowsError LispVal
readProc (outpt, [])          = readProc (outpt, [Port stdin])
readProc (_, [Port port]) = f <$> (liftIO (hGetLine port) >>= liftThrows . readExpr)
 where
  f [a] = a
  f a = List a
readProc _ = throwError $ Default "Invalid argument"

newlineProc :: (Output, [LispVal]) -> IOThrowsError LispVal
newlineProc (outpt, [])            = newlineProc (outpt, [Port stdout])
newlineProc (_, [Port port]) = liftIO $ hPutStr port "\n" >> return NoValue
newlineProc _ = throwError $ Default "Invalid argument"

writeProc :: (Output, [LispVal]) -> IOThrowsError LispVal
writeProc (outpt, [obj])            = writeProc (outpt, [obj, Port stdout])
writeProc (_, [obj, Port port]) = liftIO $ hPutStr port (show obj) >> return NoValue
writeProc _ = throwError $ Default "Invalid argument"

displayProc :: (Output, [LispVal]) -> IOThrowsError LispVal
displayProc (outpt, [obj])            = displayProc (outpt, [obj, Port stdout])
displayProc (_, [obj, Port port]) = liftIO $ hPutStr port (prettyShow obj) >> return NoValue
 where
  prettyShow (String s) = s
  prettyShow o = show o
displayProc _ = throwError $ Default "not implemented"

readContents :: (Output, [LispVal]) -> IOThrowsError LispVal
readContents (_, [String filename]) = liftM String $ liftIO $ readFile filename
readContents _ = throwError $ Default "not implemented"

readAll :: (Output, [LispVal]) -> IOThrowsError LispVal
readAll (_, [String filename]) = liftM List $ load filename
readAll _ = throwError $ Default "not implemented"

giveArgs :: (Output, [LispVal]) -> IOThrowsError LispVal
giveArgs _ = liftIO (getProgName >>= \pName -> getArgs >>= return . List . (:) (String pName) . map (String))

handlePipe :: (Output, [LispVal]) -> IOThrowsError LispVal
handlePipe (Free, args) = (liftIO $ foldM doExec (stdin) (init args)
                          >>= \hdl ->
                            getLast (last args) >>= \(lCmd, lArgs) ->
                              createProcess
                              (proc' lCmd lArgs (UseHandle hdl) Inherit)
                          >> mapM waitProcess args)
                          >> return NoValue
 where
  getLast (List (x:xs)) = return (show x, map show xs)
  getLast (Atom a) = return (a, [])
  getLast other = return (show other, [])
handlePipe (InString, args) =
  liftIO $ foldM doExec (stdin) args
    >>= \hndl -> mapM waitProcess args >> return hndl
    >>= hGetContents >>= return . ShellString . fixOutput
handlePipe _ = throwError $ Default "Not Implemented"

doExec :: Handle -> LispVal -> IO Handle
doExec handle val@(Atom _) = doExec handle (List ([val]))
doExec handle (List (fn : args)) =
  createProcess (proc' (show fn) (map show args) (UseHandle handle) CreatePipe)
  >>= getOutHandle
doExec hdl _ = return hdl
--handlePipe (InString, args) = liftIO $ pipelineProcess Pipe output fixedArgs >>= \case
--  (_, Just hout, _, _) -> hGetContents hout >>= return . ShellString . fixOutput
--  (_, _        , _, _) -> return NoValue
--  where fixedArgs = map listToCmd args

listToCmd :: LispVal -> (String, [String])
listToCmd (Atom a              ) = (a, [])
listToCmd (List (Atom a : rest)) = (a, map show rest)
listToCmd (ShellString str     ) = let ary = words str in (head ary, tail ary)
listToCmd _ = ("", [])
