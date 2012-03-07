{-  NOTICE -- THIS FILE IS AUTO-GENERATED -- DO NOT EDIT
MissingPy: Haskell Python Interface libraries
Copyright (C) 2004 - 2005 John Goerzen <jgoerzen@complete.org>

All code is under the following license unless otherwise noted:
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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

The GNU General Public License is available in the file COPYING in the source
distribution.  Debian GNU/Linux users may find this in
/usr/share/common-licenses/GPL-2.

If the GPL is unacceptable for your uses, please e-mail me; alternative
terms can be negotiated for your project.
-}
{- |
   Module     : Python.Exceptions.ExcTypes
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Python low-level exception definitions

These are definitions of the built-in Python exception objects.  You can
use them with 'MissingPy.Python.Exceptions.doesExceptionMatch' and
'MissingPy.Python.Exceptions.catchSpecificPy'.

The meanings of these exceptions can be found at
<http://www.python.org/doc/current/lib/module-exceptions.html>.

Please note that windowsError is available only on Microsoft platforms.

Written by John Goerzen, jgoerzen\@complete.org
-}
#include <Python.h>
module Python.Exceptions.ExcTypes
(

arithmeticError,
assertionError,
attributeError,
pyEOFError,
environmentError,
pyMainException,
floatingPointError,
pyIOError,
importError,
indexError,
keyError,
keyboardInterrupt,
lookupError,
memoryError,
nameError,
notImplementedError,
pyOSError,
overflowError,
referenceError,
runtimeError,
standardError,
syntaxError,
systemError,
systemExit,
typeError,
valueError,
zeroDivisionError
#ifdef MS_WINDOWS
,windowsError
#endif

)
where
import Python.Types
import Python.Objects
import System.IO.Unsafe
import Python.Utils
import Foreign hiding (unsafePerformIO)
exctypes_internal_e :: IO (Ptr CPyObject) -> IO PyObject
exctypes_internal_e f = do p <- f
                           fp <- newForeignPtr_ p
                           return $ PyObject fp

{-# NOINLINE arithmeticError #-}
arithmeticError = unsafePerformIO $ exctypes_internal_e cArithmeticError

{-# NOINLINE assertionError #-}
assertionError = unsafePerformIO $ exctypes_internal_e cAssertionError

{-# NOINLINE attributeError #-}
attributeError = unsafePerformIO $ exctypes_internal_e cAttributeError

{-# NOINLINE pyEOFError #-}
pyEOFError = unsafePerformIO $ exctypes_internal_e cEOFError

{-# NOINLINE environmentError #-}
environmentError = unsafePerformIO $ exctypes_internal_e cEnvironmentError

-- | This is Exception in Python; renamed to avoid naming conflicts here.
{-# NOINLINE pyMainException #-}
pyMainException = unsafePerformIO $ exctypes_internal_e cException

{-# NOINLINE floatingPointError #-}
floatingPointError = unsafePerformIO $ exctypes_internal_e cFloatingPointError

{-# NOINLINE pyIOError #-}
pyIOError = unsafePerformIO $ exctypes_internal_e cIOError

{-# NOINLINE importError #-}
importError = unsafePerformIO $ exctypes_internal_e cImportError

{-# NOINLINE indexError #-}
indexError = unsafePerformIO $ exctypes_internal_e cIndexError

{-# NOINLINE keyError #-}
keyError = unsafePerformIO $ exctypes_internal_e cKeyError

{-# NOINLINE keyboardInterrupt #-}
keyboardInterrupt = unsafePerformIO $ exctypes_internal_e cKeyboardInterrupt

{-# NOINLINE lookupError #-}
lookupError = unsafePerformIO $ exctypes_internal_e cLookupError

{-# NOINLINE memoryError #-}
memoryError = unsafePerformIO $ exctypes_internal_e cMemoryError

{-# NOINLINE nameError #-}
nameError = unsafePerformIO $ exctypes_internal_e cNameError

{-# NOINLINE notImplementedError #-}
notImplementedError = unsafePerformIO $ exctypes_internal_e cNotImplementedError

{-# NOINLINE pyOSError #-}
pyOSError = unsafePerformIO $ exctypes_internal_e cOSError

{-# NOINLINE overflowError #-}
overflowError = unsafePerformIO $ exctypes_internal_e cOverflowError

{-# NOINLINE referenceError #-}
referenceError = unsafePerformIO $ exctypes_internal_e cReferenceError

{-# NOINLINE runtimeError #-}
runtimeError = unsafePerformIO $ exctypes_internal_e cRuntimeError

{-# NOINLINE standardError #-}
standardError = unsafePerformIO $ exctypes_internal_e cStandardError

{-# NOINLINE syntaxError #-}
syntaxError = unsafePerformIO $ exctypes_internal_e cSyntaxError

{-# NOINLINE systemError #-}
systemError = unsafePerformIO $ exctypes_internal_e cSystemError

{-# NOINLINE systemExit #-}
systemExit = unsafePerformIO $ exctypes_internal_e cSystemExit

{-# NOINLINE typeError #-}
typeError = unsafePerformIO $ exctypes_internal_e cTypeError

{-# NOINLINE valueError #-}
valueError = unsafePerformIO $ exctypes_internal_e cValueError

{-# NOINLINE zeroDivisionError #-}
zeroDivisionError = unsafePerformIO $ exctypes_internal_e cZeroDivisionError


#ifdef MS_WINDOWS
{-# NOINLINE windowsError #-}
windowsError = unsafePerformIO $ exctypes_internal_e cWindowsError

#endif

foreign import ccall unsafe "excglue.h hspy_ArithmeticError"
 cArithmeticError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_AssertionError"
 cAssertionError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_AttributeError"
 cAttributeError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_EOFError"
 cEOFError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_EnvironmentError"
 cEnvironmentError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_Exception"
 cException :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_FloatingPointError"
 cFloatingPointError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_IOError"
 cIOError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_ImportError"
 cImportError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_IndexError"
 cIndexError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_KeyError"
 cKeyError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_KeyboardInterrupt"
 cKeyboardInterrupt :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_LookupError"
 cLookupError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_MemoryError"
 cMemoryError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_NameError"
 cNameError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_NotImplementedError"
 cNotImplementedError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_OSError"
 cOSError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_OverflowError"
 cOverflowError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_ReferenceError"
 cReferenceError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_RuntimeError"
 cRuntimeError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_StandardError"
 cStandardError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_SyntaxError"
 cSyntaxError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_SystemError"
 cSystemError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_SystemExit"
 cSystemExit :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_TypeError"
 cTypeError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_ValueError"
 cValueError :: IO (Ptr CPyObject)

foreign import ccall unsafe "excglue.h hspy_ZeroDivisionError"
 cZeroDivisionError :: IO (Ptr CPyObject)


#ifdef MS_WINDOWS
foreign import ccall unsafe "excglue.h hspy_WindowsError"
 cWindowsError :: IO (Ptr CPyObject)

#endif

