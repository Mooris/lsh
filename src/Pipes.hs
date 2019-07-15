module Pipes where

import           System.Process
import           System.IO
import           Parser
import           Pty
import           System.Posix.Process
import           Foreign.C.String

shellOps :: [(String, [LispVal] -> Handle -> IO Handle)]
shellOps = [("|", hHandlePipe)]

hHandlePipe :: [LispVal] -> Handle -> IO Handle
hHandlePipe args hout = foldl hSpwnProcess (return hout) args

handlePipe :: [LispVal] -> IO LispVal
handlePipe params =
  forkProcess son >> readSon >>= peekCString >>= return . String
 where
  son = hHandlePipe (init params) stdin
    >>= \hdl -> hSpwnProcess (return hdl) (last params) >> return ()

hSpwnProcess ::  IO Handle -> LispVal -> IO Handle
hSpwnProcess hin val = hin >>= hSpwnProcess' val

hSpwnProcess' :: LispVal -> Handle -> IO Handle
hSpwnProcess' (List (Atom func : args)) hn | Just fn <- lookup func shellOps =
  fn args hn
hSpwnProcess' (List (Atom func : args)) hn | otherwise = spwnProcess (proc func (toString args)) $ UseHandle hn
  where
    toString = map showVal
hSpwnProcess' (Atom   cmd) hn = spwnProcess (proc cmd []) $ UseHandle hn
hSpwnProcess' (String cmd) hn = spwnProcess (shell cmd) $ UseHandle hn

spwnProcess :: CreateProcess -> StdStream -> IO Handle
spwnProcess process inStream = do
  --(_, Just phout, _, pHandle) <- createProcess (proc cmd (map showVal args))
  (_, Just phout, _, pHandle) <- createProcess process { std_out = CreatePipe
                                                       , std_in = inStream
                                                       , create_group = True
                                                       }
  waitForProcess pHandle
  return phout

fixOutput :: String -> String
fixOutput output = if last output == '\n' then init output else output ++ "⏎ "
