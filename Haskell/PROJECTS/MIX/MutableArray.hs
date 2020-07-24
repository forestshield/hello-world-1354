{-
This program creates an array of 11 elements with all values initially set to 37. 
Then it reads the first element of the array. After that, the program modifies 
the first element of the array and then reads it again. 
The type declaration in the second line is necessary because our little program 
doesn't provide enough context to allow the compiler to determine the concrete type of `arr`. 
Unlike examples, real programs rarely need such declarations.

Believe it or not, now you know all that is needed to use any array type. 
Unless you are interested in speed issues, just use Array, IOArray and STArray where appropriate. 
The following topics are almost exclusively about selecting the proper array type 
to make programs run faster.
-}
import Control.Monad.ST
import Data.Array.ST
buildPair = do  arr <- newArray (1,10) 37 :: ST s (STArray s Int Int)
                a <- readArray arr 1
                writeArray arr 1 64
                b <- readArray arr 1
                return (a,b)

main = print $ runST buildPair

