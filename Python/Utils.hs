{- arch-tag: Python low-level utilities
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : Python.Utils
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Python low-level utilities

Written by John Goerzen, jgoerzen\@complete.org

Please use sparingly and with caution.  The documentation for their behavior
should be considered to be the source code.
-}

module Python.Utils (-- * Objects
                     fromCPyObject,
                     withPyObject,
                     maybeWithPyObject,
                     -- * Exceptions
                     raisePyException,
                     checkCInt,
                     -- * Environment
                     getDefaultGlobals,
                     pyImport_AddModule,
                     pyModule_GetDict,
                     py_incref
                    )
    where
import Python.Types (
                      CPyObject(..)
                    , PyObject(..)
                    , PyException(excType, excValue, excTraceBack, excFormatted, PyException)
                    )
import Python.ForeignImports (
                      cNone
                    , cpyImport_AddModule
                    , cpyModule_GetDict
                    , pyErr_Clear
                    , pyErr_Fetch
                    , pyErr_NormalizeException
                    , py_decref
                    , py_incref
                    , py_incref
                    )

import Foreign.C.Types (CInt)
import Foreign.C (withCString)
import Foreign (peek, Ptr, newForeignPtr, nullPtr, alloca, withForeignPtr)
import Control.Exception (throw)

{- | Convert a Ptr 'CPyObject' to a 'PyObject'. -}
fromCPyObject :: Ptr CPyObject -> IO PyObject
fromCPyObject po =
    if po == nullPtr
       then raisePyException
       else do fp <- newForeignPtr py_decref po
               return $ PyObject fp

{- | Called to make sure the passed CInt isn't -1.  Raise an exception if
it is. -}
checkCInt :: CInt -> IO CInt
checkCInt x = 
    if x == (-1)
       then raisePyException
       else return x

{- | Called when a Python exception has been detected.  It will raise
the exception in Haskell. -}
raisePyException :: IO a
raisePyException =
    let noneorptr cval = if cval == nullPtr
                            then do p <- cNone
                                    fromCPyObject p
                            else fromCPyObject cval
        in alloca (\typeptr -> alloca (\valptr -> alloca (\tbptr ->
       do pyErr_Fetch typeptr valptr tbptr
          pyErr_NormalizeException typeptr valptr tbptr
          ctype <- peek typeptr
          cval <- peek valptr
          ctb <- peek tbptr
          otype <- noneorptr ctype
          oval <- noneorptr cval
          otb <- noneorptr ctb
          --seq otype $ return ()
          --seq oval $ return ()
          --seq otb $ return ()
          let exc = PyException {excType = otype, excValue = oval,
                                 excTraceBack = otb,
                                 excFormatted = ""}
          pyErr_Clear
          throw exc
                   )))
{-
    do cpy <- getexc
       let (exc, val, tb) = cpy
       --pyErr_Print
       fail "Python Error!"
    where getexc = do cexc <- hspy_getexc
                      exc <- peekArray 3 cexc
                      exc2 <- mapM fromCPyObject exc
                      case exc2 of
                               [x, y, z] -> return (x, y, z)
                               _ -> fail "Got unexpected number of elements"
-}  
  
{- | Uses a 'PyObject' in a function that needs Ptr 'CPyObject'. -}
withPyObject :: PyObject -> (Ptr CPyObject -> IO b) -> IO b
withPyObject (PyObject x) = withForeignPtr x    

{- | Same as 'withPyObject', but uses nullPtr if the input is Nothing. -}
maybeWithPyObject :: Maybe PyObject -> (Ptr CPyObject -> IO b) -> IO b
maybeWithPyObject Nothing func = func nullPtr
maybeWithPyObject (Just x) y = withPyObject x y

{- | Returns the default globals environment. -}
getDefaultGlobals :: IO PyObject
getDefaultGlobals = 
    do m <- pyImport_AddModule "__main__"
       pyModule_GetDict m
       
{- | Wrapper around C PyImport_AddModule, which looks up an existing module -}
pyImport_AddModule :: String -> IO PyObject
pyImport_AddModule x =
    withCString x (\cstr -> 
        do r <- cpyImport_AddModule cstr
           py_incref r
           fromCPyObject r
                  )

{- | Gets the dict associated with a module. -}
pyModule_GetDict :: PyObject -> IO PyObject
pyModule_GetDict x =
    withPyObject x (\cpyo -> 
       do r <- cpyModule_GetDict cpyo
          py_incref r
          fromCPyObject r)

