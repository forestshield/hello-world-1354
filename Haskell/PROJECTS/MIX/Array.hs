{-
This program creates an array of 11 elements with all values initially set to 37. 
Then it reads the first element of the array. After that, the program modifies 
the first element of the array and then reads it again. 
The type declaration in the second line is necessary because our little program 
doesn't provide enough context to allow the compiler to determine the concrete type of `arr`. 
Unlike examples, real programs rarely need such declarations.
-}
import Data.Array.IO
main = do arr <- newArray (0,10) 37 :: IO (IOArray Int Int)
          a <- readArray arr 0
          writeArray arr 0 64
          d <- readArray arr 0
          writeArray arr 0 98
          b <- readArray arr 0 
          c <- readArray arr 10
          print (a,d,b,c)