{-
A storable array is an IO-mutable array which stores its contents in a contiguous memory block 
living in the C heap. Elements are stored according to the class 'Storable'. 
You can obtain the pointer to the array contents to manipulate elements from languages like C.
It is similar to 'IOUArray' (in particular, it implements the same MArray interface) but slower. 
The advantage is that it's compatible with C through the foreign function interface. 
The memory addresses of storable arrays are fixed, so you can pass them to C routines.
The pointer to the array contents is obtained by 'withStorableArray'. 
The idea is similar to 'ForeignPtr' (used internally here). 
The pointer should be used only during execution of the 'IO' action returned 
by the function passed as argument to 'withStorableArray'.
If you want to use this pointer afterwards, ensure that you call 'touchStorableArray' AFTER 
the last use of the pointer, so that the array will be not freed too early.
-}
{-# OPTIONS_GHC -fglasgow-exts #-}
import Data.Array.Storable
import Foreign.Ptr
import Foreign.C.Types
 
main = do arr <- newArray (1,10) 37 :: IO (StorableArray Int Int)
          a <- readArray arr 1
          withStorableArray arr 
            (\ptr -> memset ptr 0 40)
          b <- readArray arr 1
          print (a,b)

foreign import ccall unsafe "string.h" 
    memset  :: Ptr a -> CInt -> CSize -> IO ()
