{-# LANGUAGE OverlappingInstances#-}

{- arch-tag: Python type instances
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
   Module     : Python.Objects
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Python type instances and object utilities.

For more similar utilities, see "Python.Objects.File" and
"Python.Objects.Dict".

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Objects (
                       -- * Basic Object Types
                       PyObject,
                       -- * Conversions between Haskell and Python Objects
                       ToPyObject(..),
                       FromPyObject(..),
                       -- * Information about Python Objects
                       typeOf,
                       strOf,
                       reprOf,
                       showPyObject,
                       dirPyObject,
                       getattr,
                       hasattr,
                       setattr,
                       -- * Conversions between Python Objects
                       pyList_AsTuple,
                       -- * Calling Python Objects
                       pyObject_Call,
                       pyObject_CallHs,
                       pyObject_RunHs,
                       callMethodHs,
                       runMethodHs,
                       noParms,
                       noKwParms
                      )
where
import Python.Types
import Python.Utils
import Foreign.C.Types (
                          CLong
                        , CInt
                        , CDouble
                        )

import Foreign.C.String (
                          withCString
                        , peekCStringLen
                        , CStringLen
                        )
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (alloca)
import Python.ForeignImports (
                          cpyList_AsTuple
                        , cpyObject_Call
                        , pyDict_New
                        , pyFloat_AsDouble
                        , pyFloat_FromDouble
                        , pyInt_AsLong
                        , pyInt_FromLong
                        , pyList_Append
                        , pyList_Check
                        , pyList_GetItem
                        , pyList_New
                        , pyList_Size
                        , pyLong_FromString
                        , pyMapping_Items
                        , pyObject_Dir
                        , pyObject_GetAttrString
                        , pyObject_HasAttrString
                        , pyObject_Repr
                        , pyObject_SetAttrString
                        , pyObject_SetItem
                        , pyObject_Str
                        , pyObject_Type
                        , pyString_AsStringAndSize
                        , pyString_FromStringAndSize
                        , pyTuple_Check
                        , pyTuple_GetItem
                        , pyTuple_Size
                        )




{- | Members of this class can be converted from a Haskell type
to a Python object. -}
class ToPyObject a where
    toPyObject :: a -> IO PyObject

{- | Members of this class can be derived from a Python object. -}
class FromPyObject a where
    fromPyObject :: PyObject -> IO a

----------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------
{- | Gets the type of a Python object.  Same as type(x) in Python. -}
typeOf :: PyObject -> IO PyObject
typeOf x = withPyObject x (\pyo -> pyObject_Type pyo >>= fromCPyObject)
      
{- | Gets a string representation of a Python object.  Same 
as str(x) in Python. -}
strOf :: PyObject -> IO String
strOf x = withPyObject x 
            (\pyo -> pyObject_Str pyo >>= fromCPyObject >>= fromPyObject)

                                
{- | Gets the Python representation of a Python object.
Same as repr(x) in Python. -}
reprOf :: PyObject -> IO String
reprOf x = withPyObject x
             (\pyo -> pyObject_Repr pyo >>= fromCPyObject >>= fromPyObject)

{- | Displays a Python object and its type. -}
showPyObject :: PyObject -> IO String
showPyObject x = do typestr <- typeOf x >>= strOf
                    contentstr <- strOf x
                    return $ typestr ++ ": " ++ contentstr

{- | Displays a list of keys contained in the Python object. -}
dirPyObject :: PyObject -> IO [String]
dirPyObject x = withPyObject x (\cpyo ->
                   do dr <- pyObject_Dir cpyo >>= fromCPyObject
                      fromPyObject dr
                               )

{- | Call a Python object with all-Haskell parameters.
Similar to 'PyObject_Call'.  This limits you to a single item type for
the regular arguments and another single item type for the keyword arguments. 
Nevertheless, it could be a handy shortcut at times.

For a higher-level wrapper, see 'Python.Interpreter.callByName'.

You may find 'noParms' and 'noKwParms' useful if you aren't passing any
parameters. -}
pyObject_CallHs :: (ToPyObject a, ToPyObject b, FromPyObject c) =>
                   PyObject     -- ^ Object t
                -> [a]          -- ^ List of non-keyword parameters
                -> [(String, b)] -- ^ List of keyword parameters
                -> IO c          -- ^ Return value
pyObject_CallHs callobj simpleargs kwargs =
    pyObject_Hs callobj simpleargs kwargs >>= fromPyObject

pyObject_Hs :: (ToPyObject a, ToPyObject b) =>
                   PyObject     -- ^ Object t
                -> [a]          -- ^ List of non-keyword parameters
                -> [(String, b)] -- ^ List of keyword parameters
                -> IO PyObject         -- ^ Return value
pyObject_Hs callobj simpleargs kwargs =
    let conv (k, v) = do v1 <- toPyObject v
                         return (k, v1)
        in
        do s <- mapM toPyObject simpleargs
           k <- mapM conv kwargs
           pyObject_Call callobj s k

{- | Like 'PyObject_CallHs', but discards the return value. -}
pyObject_RunHs :: (ToPyObject a, ToPyObject b) =>
                   PyObject     -- ^ Object t
                -> [a]          -- ^ List of non-keyword parameters
                -> [(String, b)] -- ^ List of keyword parameters
                -> IO ()         -- ^ Return value
pyObject_RunHs callobj simpleargs kwargs =
    pyObject_Hs callobj simpleargs kwargs >> return ()

callMethodHs_internal :: (ToPyObject a, ToPyObject b) =>
                         PyObject
                      -> String
                      -> [a]
                      -> [(String, b)]
                      -> IO PyObject
callMethodHs_internal pyo method args kwargs =
    do mobj <- getattr pyo method
       pyObject_Hs mobj args kwargs
                            
{- | Calls the named method of the given object. -}
callMethodHs :: (ToPyObject a, ToPyObject b, FromPyObject c) =>
                PyObject        -- ^ The main object
             -> String          -- ^ Name of method to call
             -> [a]             -- ^ Non-kw args
             -> [(String, b)]   -- ^ Keyword args
             -> IO c            -- ^ Result
callMethodHs pyo method args kwargs =
    callMethodHs_internal pyo method args kwargs >>= fromPyObject

{- | Like 'callMethodHs', but discards the return value. -}
runMethodHs :: (ToPyObject a, ToPyObject b) =>
                PyObject        -- ^ The main object
             -> String          -- ^ Name of method to call
             -> [a]             -- ^ Non-kw args
             -> [(String, b)]   -- ^ Keyword args
             -> IO ()            -- ^ Result
runMethodHs pyo method args kwargs =
    callMethodHs_internal pyo method args kwargs >> return ()

noParms :: [String]
noParms = []

noKwParms :: [(String, String)]
noKwParms = []



{- | Call a Python object (function, etc).

For a higher-level wrapper, see 'Python.Interpreter.callByName'.
 -}
pyObject_Call :: PyObject       -- ^ Object to call
              -> [PyObject]     -- ^ List of non-keyword parameters (may be empty)
              -> [(String, PyObject)] -- ^ List of keyword parameters (may be empty)
              -> IO PyObject    -- ^ Return value
pyObject_Call callobj simpleparams kwparams =
        do pyosimple <- toPyObject simpleparams >>= pyList_AsTuple
           pyokw <- toPyObject kwparams
           cval <- withPyObject callobj (\ccallobj ->
                    withPyObject pyosimple (\cpyosimple ->
                     withPyObject pyokw (\cpyokw ->
                      cpyObject_Call ccallobj cpyosimple cpyokw)))
           fromCPyObject cval
       
-- ^ Converts a Python list to a tuple.
pyList_AsTuple :: PyObject -> IO PyObject
pyList_AsTuple x =
    withPyObject x (\cpo -> cpyList_AsTuple cpo >>= fromCPyObject)

{- | An interface to a function similar to Python's getattr.  This will
look up an attribute (such as a method) of an object. -}
getattr :: PyObject -> String -> IO PyObject
getattr pyo s =
    withPyObject pyo (\cpo ->
     withCString s (\cstr ->
      pyObject_GetAttrString cpo cstr >>= fromCPyObject))

{- | An interface to Python's hasattr.  Returns True if the named
attribute exists; False otherwise. -}
hasattr :: PyObject -> String -> IO Bool
hasattr pyo s =
    withPyObject pyo (\cpo ->
     withCString s (\cstr ->
      do r <- pyObject_HasAttrString cpo cstr >>= checkCInt
         if r == 0
            then return False
            else return True
                   )
                     )
{- | An interface to Python's setattr, used to set attributes of an object.
-}
setattr :: PyObject             -- ^ Object to operate on
        -> String               -- ^ Name of attribute
        -> PyObject             -- ^ Set the attribute to this value
        -> IO ()
setattr pyo s setpyo =
    withPyObject pyo (\cpo ->
     withPyObject setpyo (\csetpyo ->
      withCString s (\cstr ->
       pyObject_SetAttrString cpo cstr csetpyo >>= checkCInt >> return ()
                    )))

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

-- FIXME: ERROR CHECKING!

--------------------------------------------------
-- [PyObject] Lists

-- | Lists from a PyObject
instance ToPyObject [PyObject] where
    toPyObject mainlist =
        do l <- pyList_New 0
           mapM_ (\pyo -> withPyObject pyo (\x -> pyList_Append l x >>= checkCInt)) mainlist
           fromCPyObject l

-- | Tuples and Lists to [PyObject] lists
instance FromPyObject [PyObject] where
    fromPyObject x = 
        let worker cpyo =
                do islist <- pyList_Check cpyo >>= checkCInt
                   istuple <- pyTuple_Check cpyo >>= checkCInt
                   if islist /= 0
                      then fromx pyList_Size pyList_GetItem cpyo
                      else if istuple /= 0
                                 then fromx pyTuple_Size pyTuple_GetItem cpyo
                                 else fail "Error fromPyObject to [PyObject]: Passed object not a list or tuple."
            fromx sizefunc itemfunc cpyo = do size <- sizefunc cpyo
                                              fromx_worker 0 size itemfunc cpyo
            fromx_worker counter size itemfunc cpyo =
                if counter >= size 
                   then return []
                   else do thisitem <- itemfunc cpyo counter
                           py_incref thisitem
                           thisobj <- fromCPyObject thisitem
                           {- This unsafeInterlaveIO caused segfaults.  Theory:
                              parent object would be deallocated before all
                              items would be consumed. -}
                           next <- {-unsafeInterleaveIO $-} fromx_worker (succ counter) size itemfunc cpyo
                           return $ thisobj : next
            in
            withPyObject x worker

--------------------------------------------------
-- Association Lists

-- | Dicts from ALs
instance ToPyObject [(PyObject, PyObject)] where
    toPyObject mainlist =
        do d <- pyDict_New
           mapM_ (setitem d) mainlist
           fromCPyObject d
        where setitem l (key, value) =
                  withPyObject key (\keyo ->
                      withPyObject value (\valueo ->
                          pyObject_SetItem l keyo valueo >>= checkCInt))

-- | ALs from Dicts
instance FromPyObject [(PyObject, PyObject)] where
    fromPyObject pydict = withPyObject pydict (\cpydict ->
           -- Type sigs here are for clarity only
        do -- This gives a PyObject
           items <- (pyMapping_Items cpydict >>= fromCPyObject):: IO PyObject
           -- Now, make a Haskell [[PyObject, PyObject]] list
           itemlist <- (fromPyObject items)::IO [[PyObject]]
           -- Finally, convert it to a list of tuples.
           return $ map list2tup itemlist
                                              )
        where list2tup x = case x of
                                  x1:x2:[] -> (x1, x2)
                                  _ -> error "Expected 2-tuples in fromPyObject dict"
                                       
-- | This is a common variant used for arg lists
instance ToPyObject a => ToPyObject [(a, PyObject)] where
    toPyObject mainlist =
        let conv (k, v) = do k1 <- toPyObject k
                             return (k1, v)
            in mapM conv mainlist >>= toPyObject
instance FromPyObject a => FromPyObject [(a, PyObject)] where
    fromPyObject pyo =
        let conv (k, v) = do k1 <- fromPyObject k
                             return (k1, v)
        in do list <- (fromPyObject pyo)::IO [(PyObject, PyObject)]
              mapM conv list


-- | Dicts from Haskell objects
instance (ToPyObject a, ToPyObject b) => ToPyObject [(a, b)] where
    toPyObject mainlist =
        let convone (i1, i2) = do oi1 <- toPyObject i1
                                  oi2 <- toPyObject i2
                                  return (oi1, oi2)
        in do newl <- mapM convone mainlist
              toPyObject newl

-- | Dicts to Haskell objects
instance (FromPyObject a, FromPyObject b) => FromPyObject [(a, b)] where
    fromPyObject pydict =
        let conv (x, y) = do x1 <- fromPyObject x
                             y1 <- fromPyObject y
                             return (x1, y1)
            in do pyodict <- ((fromPyObject pydict)::IO [(PyObject, PyObject)])
                  mapM conv pyodict

--------------------------------------------------
-- Strings

-- CStringLen to PyObject.  Use CStringLen to handle embedded nulls.
instance ToPyObject CStringLen where
   toPyObject (x, len) = 
       pyString_FromStringAndSize x (fromIntegral len) >>= fromCPyObject

-- String to PyObject
instance ToPyObject String where
    toPyObject x = withCString x (\cstr -> toPyObject (cstr, length x))

-- PyObject to String
instance FromPyObject String where
    fromPyObject x = withPyObject x (\po ->
        alloca (\lenptr ->
           alloca (\strptr ->
            do pyString_AsStringAndSize po strptr lenptr
               len <- peek lenptr
               cstr <- peek strptr
               peekCStringLen (cstr, (fromIntegral) len)
                  )
               )
                                    )

--------------------------------------------------
-- Numbers, Python Ints

-- Python ints are C longs
instance ToPyObject CLong where
    toPyObject x =  pyInt_FromLong x >>= fromCPyObject

-- And convert back.
instance FromPyObject CLong where
    fromPyObject x = withPyObject x pyInt_AsLong

-- We'll also support CInts.
instance ToPyObject CInt where
    toPyObject x = toPyObject ((fromIntegral x)::CLong)

instance FromPyObject CInt where
    fromPyObject x = do y <- (fromPyObject x)::IO CLong
                        return $ fromIntegral y

--------------------------------------------------
-- Numbers, Python Longs

instance ToPyObject Integer where
    toPyObject i = 
        -- Use strings here since no other C type supports
        -- unlimited precision.
        let repr = show i
        in withCString repr (\cstr -> 
             pyLong_FromString cstr nullPtr 10 >>= fromCPyObject)
                                 
instance FromPyObject Integer where
    fromPyObject pyo = 
        do longstr <- strOf pyo
           return $ read longstr

--------------------------------------------------
-- Numbers, anything else.
{- For these, we attempt to guess whether to handle it as an
int or a long. -}
{-
Disabled for now; this is a low-level interface, and it seems to be overly
complex for this.

instance Integral a => ToPyObject a where
    toPyObject x =
        let intval = toInteger x
            in
            if (intval < (toInteger (minBound::CLong)) ||
                intval > (toInteger (maxBound::CLong)))
                then toPyObject intval
                else toPyObject ((fromIntegral x)::CLong)

-- On the return conversion, we see what the bounds for
-- the desired type are, and treat it thusly.
instance (Bounded a, Integral a) => FromPyObject a where
    fromPyObject x =
        let minpyint = toInteger (minBound::CLong)
            maxpyint = toInteger (maxBound::CLong)
            minpassed = toInteger (minBound::a)
            maxpassed = toInteger (maxBound::a)
            in if (minpassed < minpyint || maxpassed > maxpyint)
                  then do intval <- fromPyObject x
                          return $ fromInteger intval
                  else do longval <- ((fromPyObject x)::IO CLong)
                          return $ fromIntegral longval

-}

--------------------------------------------------
-- Floating-Point Values

instance ToPyObject CDouble where
    toPyObject x = pyFloat_FromDouble x >>= fromCPyObject

instance FromPyObject CDouble where
    fromPyObject x = withPyObject x pyFloat_AsDouble

-- | Lists from anything else
instance ToPyObject a => ToPyObject [a] where
    toPyObject mainlist = 
        do newlist <- mapM toPyObject mainlist
           toPyObject newlist

instance FromPyObject a => FromPyObject [a] where
    fromPyObject pylistobj = 
        do pylist <- fromPyObject pylistobj
           mapM fromPyObject pylist


