{-# LANGUAGE OverlappingInstances#-}

{- arch-tag: Python dict-like objects
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
   Module     : Python.Objects.Dict
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Python dict-like objects

Written by John Goerzen, jgoerzen\@complete.org

This module can be used to access Python dicts and dict-like objects such as
dbm databases.  For a higher-level interface to creating and working with these
dbm interfaces, please see the functions in "MissingPy.AnyDBM".  Also,
for functions that use this, please see "Database.AnyDBM".

-}

module Python.Objects.Dict (PyDict,
                            mkPyDict,
                            fromPyDict)
where
import Python.ForeignImports (pyObject_SetItem, pyObject_DelItem, pyObject_GetItem, pyErr_Clear, pyMapping_Keys)
import Python.Objects(
                              toPyObject
                            , fromPyObject
                            , hasattr
                            , noKwParms
                            , noParms
                            , runMethodHs
                            , toPyObject
                            )
import Python.Utils (withPyObject, checkCInt, fromCPyObject)
import Foreign (Ptr, nullPtr)
import Python.Exceptions (catchPy, exc2ioerror)
import Database.AnyDBM (AnyDBM (..))
import Python.Types (PyObject, CPyObject)

{- | The basic type for a Python dict or dict-like object. -}
newtype PyDict = PyDict 
    PyObject                    -- Main dict object

{- | Takes a 'PyObject' representing a Python dict or dict-like objext
and makes it into a 'PyDict'. -}
mkPyDict :: PyObject -> PyDict
mkPyDict o = PyDict o

{- | Takes a 'PyDict' and returns its internal 'PyObject'. -}
fromPyDict :: PyDict -> PyObject
fromPyDict (PyDict o) = o

{- | Wrap an operation, raising exceptions in the IO monad as appropriate. -}
pydwrap :: PyDict -> (PyObject -> IO a) -> IO a
pydwrap (PyDict pyobj) func = catchPy (func pyobj) exc2ioerror

{- | Give it a CPyObject instead. -}
cpydwrap :: PyDict -> (Ptr CPyObject -> IO a) -> IO a
cpydwrap x func = pydwrap x (\y -> withPyObject y func)

instance AnyDBM PyDict where
    insertA h k v = 
        do ko <- toPyObject k
           kv <- toPyObject v
           withPyObject ko (\ck ->
            withPyObject kv (\cv ->
             cpydwrap h (\cdict ->
              pyObject_SetItem cdict ck cv >>= checkCInt >> return()
                     )
                            )
                           )
    deleteA h k = 
        do ko <- toPyObject k
           withPyObject ko (\ck ->
            cpydwrap h (\cdict ->
             pyObject_DelItem cdict ck >>= checkCInt >> return ()))
    lookupA h k =
        do ko <- toPyObject k
           withPyObject ko (\ck ->
            cpydwrap h (\cdict ->
             do r <- pyObject_GetItem cdict ck
                if r == nullPtr
                   then do pyErr_Clear -- Ignore this exception
                           return Nothing
                   else do retval <- fromCPyObject r >>= fromPyObject
                           return $ Just retval
                                  ))
    {-
    toListA h =
        This used to be:
        pydwrap h fromPyObject
        but some *dbm's are incompatible with that.  Sigh. -}

    keysA h =
        cpydwrap h (\ch ->
         do keysobj <- pyMapping_Keys ch >>= fromCPyObject
            keys <- (fromPyObject keysobj)::IO [String]
            return keys
                   )

    flushA h = pydwrap h (\pyo -> do h <- hasattr pyo "sync"
                                     if h 
                                        then runMethodHs pyo "sync" noParms noKwParms
                                        else return ()
                         )

    closeA h = pydwrap h (\pyo -> do h <- hasattr pyo "close"
                                     if h
                                        then runMethodHs pyo "close" noParms noKwParms
                                        else return ()
                         )

                                        
            
    
