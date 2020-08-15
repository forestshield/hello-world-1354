import System.Environment   
import Data.List  
  
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName

--build exe using ghc
--   	stack ghc -- Main.hs -o testprogram2
--  	stack ghc first_prog.hs

-------------------
-- this is output
{-
./arg-test one two w00t "multi word arg" [1,2,3]
The arguments are:
one
two
w00t
multi word arg
[1,2,3]
The program name is:
arg-test
-}

















