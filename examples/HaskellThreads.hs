{- Example demonstrating multiple haskell threads calling into python runtime -}

import Text.Printf
import Python.Exceptions
import Python.Interpreter 
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

pythonFunction :: MVar Int -> Int -> IO ()
pythonFunction done n = do
  finally (withGIL $ handlePy exc2ioerror $ do pyRun_SimpleString $ printf "print '%d'" n)
          (do modifyMVar_ done $ return . (+1)) -- signal that thread is finished

untilM :: IO Bool -> IO ()
untilM pred = do
  v <- pred
  if v then return () else yield >> untilM pred

main = do
  py_initialize
  done <- newMVar 0
  let n = 40
  mapM_ (forkIO . pythonFunction done) [1..(n :: Int)]

  -- wait for threads to finish
  untilM $ do
    d <- readMVar done
    return $ d == n
  
