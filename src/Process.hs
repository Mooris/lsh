{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Process where

import           LispVal

import qualified Language.C.Inline             as C
import           GHC.IO.Handle
import           System.Process


C.include "<stddef.h>"
C.include "<sys/wait.h>"

data Chaining = None | Pipe

getOutHandle
  :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO Handle
getOutHandle (_, Just h, _, _) = return h

getProcessHandle
  :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -> IO ProcessHandle
getProcessHandle (_, _, _, h) = return h

fixOutput :: String -> String
fixOutput output = if last output /= '\n' then output ++ "⏎ " else output

waitProcess :: a -> IO a
waitProcess a = [C.exp| void { wait(NULL) } |] >> return a

proc' :: FilePath -> [String] -> StdStream -> StdStream -> CreateProcess
proc' cmd args sin sout = CreateProcess {  cmdspec = RawCommand cmd args,
                                  cwd = Nothing,
                                  env = Nothing,
                                  std_in = sin,
                                  std_out = sout,
                                  std_err = Inherit,
                                  close_fds = False,
                                  create_group = False,
                                  delegate_ctlc = False,
                                  detach_console = False,
                                  create_new_console = False,
                                  new_session = False,
                                  child_group = Nothing,
                                  child_user = Nothing,
                                  use_process_jobs = False}

pipelineProcess
  :: Chaining
  -> StdStream
  -> [(String, [String])]
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
pipelineProcess _ stdOut [(cmd, args)] = createProcess (proc' cmd args Inherit stdOut) >>= waitProcess
pipelineProcess Pipe stdOut args =
  pipelineProcess' args stdOut Inherit >>= \end -> mapM waitProcess args >> return end
 where
  pipelineProcess' ((cmd, rest) : []) output input = createProcess (proc' cmd rest input output)
  pipelineProcess' ((cmd, rest) : rst) output input = createProcess (proc' cmd rest input CreatePipe) >>= getOutHandle >>= (pipelineProcess' rst output) . UseHandle

handleSimpleCommand :: String -> [LispVal] -> IO LispVal
handleSimpleCommand cmdString args =
  pipelineProcess None Inherit [(cmdString, mappedArgs)] >> return NoValue
 where
  mappedArgs = map show args

handleCommand :: Output -> String -> [LispVal] -> IO LispVal
handleCommand Free cmdString args =
  pipelineProcess None Inherit [(cmdString, mappedArgs)] >> return NoValue
 where
  mappedArgs = map show args
handleCommand InString cmdString args =
  pipelineProcess None CreatePipe [(cmdString, mappedArgs)]
    >>= getOutHandle
    >>= hGetContents
    >>= (return . ShellString . fixOutput)
 where
  mappedArgs = map show args

-- handleCommand :: String -> [LispVal] -> IO LispVal
-- handleCommand cmdString args =
--   let
--     mappedArgs = map show args
--   in forkProcess (forkaa mappedArgs) >> readSon >>= peekCString >>= return . String . fixOutput
--   where
--     forkaa mappedArgs = setupSon
--       >>  pipelineProcess None [(cmdString, mappedArgs)]
--       >>= getProcessHandle
--       >>= waitForProcess
--       >>  return ()

-- handlePipe :: [LispVal] -> IO LispVal
-- handlePipe args = forkProcess forkaa >> readSon >>= peekCString >>= return . String . fixOutput
--  where
--     forkaa = setupSon
--         >>  pipelineProcess Pipe (setupPipeline args)
--         >>= getProcessHandle
--         >>= waitForProcess
--         >>  return ()
--     setupPipeline = map listToCmd

