{-# LANGUAGE OverlappingInstances#-}

{- arch-tag: Python file-like objects
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
   Module     : Python.Objects.File
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Python file-like objects

Written by John Goerzen, jgoerzen\@complete.org

This module provides a Haskell interface to work with Python file-like objects.
The Haskell interface is a "System.IO.HVIO" interface, which is similar
in concept to the Python file-like object system.

You can create such objects by using 'openPyFile' from this module, or
'MissingPy.FileArchive.GZip.openGz' or 'MissingPy.FileArchive.BZip2.openBz2'.

Functions that you can use to operate on these objects are defined at
"System.IO.HVIO".
-}

module Python.Objects.File (-- * PyFile Objects
                            PyFile,
                            mkPyFile,
                            fromPyFile,
                            openPyFile,
                            pyfwrap,
                            openModeConv
                      )
where
import Python.Objects (       PyObject(..)
                            , callMethodHs
                            , fromPyObject
                            , getattr
                            , hasattr
                            , noKwParms
                            , noParms
                            , runMethodHs
                            , showPyObject
                            , toPyObject
                            )
import Python.Interpreter (callByName)
import System.IO (IOMode(..), SeekMode(..))
import System.IO.Error (eofErrorType)
import System.IO.Unsafe (unsafeInterleaveIO)
import Python.Exceptions (catchPy, exc2ioerror)
import System.IO.HVIO (HVIO(..))
import Foreign.C.Types (CInt, CLong)

{- | The basic type for a Python file or file-like object.

'PyFile's are a member of System.IO.HVIO and can be used as any other
Haskell HVFS object such as a Handle.

'PyFile' objects cannot reliably detect EOF when asked by 'vIsEOF', but
can detect it and raise the appropriate IOError when it is reached.
Also, 'PyFile' objects cannot determine if they are readable, writable,
or seekable in advance.
 -}
newtype PyFile = PyFile PyObject

{- | Takes a 'PyObject' representing a Python file or file-like object
and makes it into a 'PyFile'. -}
mkPyFile :: PyObject -> PyFile
mkPyFile o = PyFile o

{- | Extracts the 'PyObject' representing this 'PyFile'. -}
fromPyFile :: PyFile -> PyObject
fromPyFile (PyFile o) = o

{- | Convert a Haskell open mode to a Python mode string -}
openModeConv ReadMode = "r"
openModeConv WriteMode = "w"
openModeConv AppendMode = "a"
openModeConv ReadWriteMode = "w+"

{- | Open a file on disk and return a 'PyFile'. -}
openPyFile :: FilePath -> IOMode -> IO PyFile
openPyFile fp mode =
           do parms1 <- toPyObject [fp]
              parms2 <- toPyObject [openModeConv mode]
              obj <- callByName "open" [parms1, parms2] []
              return $ mkPyFile obj

------------------------------------------------------------
-- HVIO
------------------------------------------------------------

instance Show PyFile where
    show _ = "<Python File Object>"

{- | Wrap an operation, raising exceptions in the IO monad as appropriate. -}
pyfwrap :: PyFile -> (PyObject -> IO a) -> IO a
pyfwrap (PyFile pyobj) func = catchPy (func pyobj) exc2ioerror

raiseEOF :: PyFile -> IO a
raiseEOF h = vThrow h eofErrorType

instance HVIO PyFile where
    vClose pyf = pyfwrap pyf (\pyo -> runMethodHs pyo "close" noParms noKwParms)
    
    vIsClosed pyf = pyfwrap pyf (\pyo ->
                      do h <- hasattr pyo "closed"
                         if h then
                            do v <- (getattr pyo "closed" >>= fromPyObject)::IO CInt
                               if v == 0
                                  then return False
                                  else return True
                            else return False -- Don't know; fake it.
                                )

    vGetContents pyf = do vTestOpen pyf
                          vTestEOF pyf
                          pyfwrap pyf (\pyo ->
                           let loop = unsafeInterleaveIO $
                                do block <- callMethodHs pyo "read" 
                                            [4096::CLong] noKwParms
                                   case block of
                                     [] -> do vClose pyf
                                              return []
                                     x -> do next <- loop
                                             return $ x : next
                           in do c <- loop
                                 return $ concat c
                                                        )


    -- Have to fake it.  We have no EOF indication.
    vIsEOF pyf = return False

    vShow pyf = pyfwrap pyf showPyObject

    vGetChar pyf = do vTestOpen pyf
                      pyfwrap pyf (\pyo ->
                        do c <- callMethodHs pyo "read" [1::CInt] noKwParms
                           case c of 
                                [] -> raiseEOF pyf
                                [x] -> return x
                               )

    vGetLine pyf = do vTestOpen pyf
                      pyfwrap pyf (\pyo ->
                       do line <- callMethodHs pyo "readline" noParms noKwParms
                          case reverse line of
                                   [] -> raiseEOF pyf
                                   '\n':xs -> return $ reverse xs
                                   x -> return line
                               )

    vPutChar pyf c = vPutStr pyf [c]

    {- Python strings are non-lazy, so process these in chunks. -}
    vPutStr pyf [] = vTestOpen pyf >> return ()
    vPutStr pyf s = let (this, next) = (splitAt 4096 s)::(String, String)
                        in do vTestOpen pyf
                              pyfwrap pyf (\pyo ->
                                     runMethodHs pyo "write" [this] noKwParms)
                              vPutStr pyf next

    vFlush pyf = pyfwrap pyf (\pyo ->
                     do vTestOpen pyf
                        h <- hasattr pyo "flush"
                        if h then runMethodHs pyo "flush" noParms noKwParms
                           else return ()
                             )

    {- Some file-like objects don't take an offset.  Sigh. -}
    vSeek pyf sm offset =
        let seekint = case sm of
                           AbsoluteSeek -> 0::CLong
                           RelativeSeek -> 1
                           SeekFromEnd -> 2
            in do vTestOpen pyf
                  pyfwrap pyf (\pyo -> 
                   case sm of
                       AbsoluteSeek -> runMethodHs pyo "seek" 
                                         [(fromIntegral offset)::CLong]
                                         noKwParms
                       _ -> runMethodHs pyo "seek" [(fromIntegral offset), 
                                                    seekint] noKwParms
                           )

    vTell pyf = pyfwrap pyf (\pyo ->
                 vTestOpen pyf >> callMethodHs pyo "tell" noParms noKwParms)

    vIsSeekable _ = return True -- fake it
    vIsWritable _ = return True -- fake it
    vIsReadable _ = return True -- fake it
    
