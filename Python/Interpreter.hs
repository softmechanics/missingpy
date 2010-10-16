{-# LANGUAGE CPP, OverlappingInstances #-}

{- arch-tag: Python interpreter module
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
   Module     : Python.Interpreter
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Interface to Python interpreter

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Interpreter (
                          py_initialize,
                          py_initializeThreaded,
                          -- * Intrepreting Code
                          pyRun_SimpleString,
                          pyRun_String,
                          pyRun_StringHs,
                          StartFrom(..),
                          -- * Calling Code
                          callByName,
                          callByNameHs,
                          noParms,
                          noKwParms,
                          -- * Imports
                          pyImport,
                          pyImport_ImportModule,
                          pyImport_AddModule,
                          pyModule_GetDict,
                          -- * Threads
#ifndef PYTHON_PRE_2_3            
                          cpy_GILEnsure,
                          cpy_GILRelease,
                          withGIL,
#endif
                          )
where

import Python.Utils (
                      checkCInt
                    , getDefaultGlobals
                    , withPyObject
                    , fromCPyObject
                    , py_incref
                    , pyModule_GetDict
                    , pyImport_AddModule
                    )

import Python.Objects (
                      toPyObject
                    , noParms
                    , noKwParms
                    , fromPyObject
                    , pyObject_Call
                    , pyObject_CallHs
                    , PyObject
                    , ToPyObject
                    , FromPyObject
                    )

import Python.Types (StartFrom(..))

import Python.ForeignImports (
                      cpy_initialize
                    , cpy_InitThreads
                    , cpyRun_String
                    , cpyRun_SimpleString
                    , cpyImport_ImportModuleEx
                    , sf2c
                    , pyImport_GetModuleDict
                    , pyDict_SetItemString
#ifndef PYTHON_PRE_2_3                    
                    , cpy_GILEnsure
                    , cpy_GILRelease
#endif
                    )

import Foreign.C (withCString)

{- | Initialize the Python interpreter environment.

MUST BE DONE BEFORE DOING ANYTHING ELSE! -}
py_initialize :: IO ()
py_initialize = do cpy_initialize
                   pyImport "traceback"

py_initializeThreaded :: IO ()
py_initializeThreaded = do cpy_InitThreads
                           py_initialize
                           
#ifndef PYTHON_PRE_2_3
withGIL :: IO a -> IO a                           
withGIL act = do st <- cpy_GILEnsure
                 r <- act
                 cpy_GILRelease st
                 return r
#endif                 

pyRun_SimpleString :: String -> IO ()
pyRun_SimpleString x = withCString x (\cs ->
                                          do cpyRun_SimpleString cs >>= checkCInt
                                             return ()
                                     )

-- | Like 'pyRun_String', but take more Haskellish args and results.
pyRun_StringHs :: (ToPyObject b, FromPyObject c) =>
                  String        -- ^ Command to run
               -> StartFrom     -- ^ Start token
--               -> [(String, a)] -- ^ Globals (may be empty)
               -> [(String, b)] -- ^ Locals (may be empty)
               -> IO c
pyRun_StringHs cmd start locals =
    let conv (k, v) = do v1 <- toPyObject v
                         return (k, v1)
        in do
           --rglobals <- mapM conv globals
           rlocals <- mapM conv locals
           pyRun_String cmd start rlocals >>= fromPyObject
    
-- | Run some code in Python.
pyRun_String :: String          -- ^ Command to run
             -> StartFrom       -- ^ Start Token
--             -> [(String, PyObject)] -- ^ Globals (may be empty)
             -> [(String, PyObject)] -- ^ Locals (may be empty)
             -> IO PyObject     -- ^ Result
pyRun_String command startfrom xlocals =
    let cstart = sf2c startfrom
        in do dobj <- getDefaultGlobals
              rlocals <- toPyObject xlocals
              withCString command (\ccommand ->
               withPyObject dobj (\cglobals ->
                withPyObject rlocals (\clocals ->
                 cpyRun_String ccommand cstart cglobals clocals >>= fromCPyObject
                              )))

{- | Call a function or callable object by name. -}
callByName :: String            -- ^ Object\/function name
           -> [PyObject]        -- ^ List of non-keyword parameters
           -> [(String, PyObject)] -- ^ List of keyword parameters
           -> IO PyObject
callByName fname sparms kwparms =
    do func <- pyRun_String fname Py_eval_input []
       pyObject_Call func sparms kwparms

{- | Call a function or callable object by namem using Haskell args
and return values..

You can use 'noParms' and 'noKwParms' if you have no simple or
keyword parameters to pass, respectively. -}
callByNameHs :: (ToPyObject a, ToPyObject b, FromPyObject c) =>
              String            -- ^ Object\/function name
           -> [a]               -- ^ List of non-keyword parameters
           -> [(String, b)]     -- ^ List of keyword parameters
           -> IO c
callByNameHs fname sparms kwparms =
    do func <- pyRun_String fname Py_eval_input []
       pyObject_CallHs func sparms kwparms


{- | Import a module into the current environment in the normal sense
(similar to \"import\" in Python).
-}
pyImport :: String -> IO ()
pyImport x = 
    do pyImport_ImportModule x 
       globals <- getDefaultGlobals
       cdict <- pyImport_GetModuleDict
       py_incref cdict
       pyo2 <- fromCPyObject cdict
       dict <- fromPyObject pyo2
       case lookup x dict of
           Nothing ->  return ()
           Just pyo -> do withPyObject globals (\cglobals ->
                           withPyObject pyo (\cmodule ->
                            withCString x (\cstr ->
                             pyDict_SetItemString cglobals cstr cmodule >>= checkCInt)))
                          return ()

{- | Wrapper around C PyImport_ImportModule, which imports a module.

You may want the higher-level 'pyImport' instead. -}
pyImport_ImportModule :: String -> IO PyObject
pyImport_ImportModule x =
    do globals <- getDefaultGlobals
       fromlist <- toPyObject ['*']
       cr <- withPyObject globals (\cglobals ->
              withPyObject fromlist (\cfromlist ->
               withCString x (\cstr -> 
                cpyImport_ImportModuleEx cstr cglobals cglobals cfromlist)))
       fromCPyObject cr

