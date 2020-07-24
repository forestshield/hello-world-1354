{-

THIS EX DOES NOT COMPILE !!!

Additional comments: GHC 6.6 made access to 'StorableArray' as fast as to any other unboxed arrays. 
The only difference between 'StorableArray' and 'UArray' is that UArray lies 
in relocatable part of GHC heap while 'StorableArray' lies 
in non-relocatable part and therefore keep the fixed address, 
what allow to pass this address to the C routines and save it in the C data structures.

GHC 6.6 also adds an 'unsafeForeignPtrToStorableArray' operation that allows the use of any Ptr 
as the address of a 'StorableArray' and in particular works with arrays returned by C routines. 
Here is an example of using this operation:

This example allocates memory for 10 Ints (which emulates an array returned by some C function), 
then converts the returned 'Ptr Int' to 'ForeignPtr Int' 
and 'ForeignPtr Int' to 'StorableArray Int Int'. 
It then writes and reads the first element of the array. 
At the end, the memory used by the array is deallocated by 'free', 
which again emulates deallocation by C routines. 
We can also enable the automatic freeing of the allocated block by replacing "newForeignPtr_ ptr" 
with "newForeignPtr finalizerFree ptr". In this case memory will be automatically 
freed after the last array usage, as for any other Haskell objects.
-}

import Data.Array.Storable
import Data.Array.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr

main = do ptr <- mallocArray 10
          fptr <- newForeignPtr_ ptr
          arr <- unsafeForeignPtrToStorableArray (1,10) fptr :: IO (StorableArray Int Int)
          writeArray arr 1 64
          a <- readArray arr 1
          print a
          free ptr

