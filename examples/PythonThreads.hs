import Python.Interpreter
import Python.Exceptions

import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf

{- Example demonstrating a multithreaded python program.
 - Python runs on haskell's threads, so we periodically 
 - acquire the Global Interpreter Lock to make some progress,
 - releasing it again so other haskell threads can call into
 - python (e.g. to spawn more python threads).
 -
 - to run, add the examples folder to PYTHONPATH, or else PyMultiThread module
 - won't be found.
 -}


{- This is a hack to get the python runtime to run suspended
 - python threads (using GHC's thread).  There may be a better
 - way to do this.
 -}
poller = forever $ do
  withGIL $ pyRun_SimpleString "time.sleep(1)"
  yield

pyNewThread :: Int -> IO ()
pyNewThread n = do
  withGIL $ pyRun_SimpleString $ printf "PythonThreads.newThread(%d)" n
  printf "spawned thread %d\n" n

main = do 
  py_initializeThreaded 
  handlePy (\e -> print =<< formatException e) $ do
    pyImport "PythonThreads"
    pyImport "time"
    pyNewThread 1
    pyNewThread 2

    -- nothing happens until we start poller
    threadDelay 2000000
    forkIO $ poller
    threadDelay 10000000

