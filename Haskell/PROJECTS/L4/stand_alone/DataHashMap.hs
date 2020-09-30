import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy
import Data.Char

hashMap = fromList [(1 :: Int, 'a'), (2, 'b'), (3, 'c')]

main = do
    print $ hashMap                       -- fromList [(1,'a'),(2,'b'),(3,'c')]      
    print $ keys hashMap                  -- [1,2,3]                                    
    print $ elems hashMap                 -- "abc"                                      
    print $ null hashMap                  -- False                                         
    print $ size hashMap                  -- 3                                               
    print $ member 1 hashMap              -- True                                               
    print $ member 5 hashMap              -- False                                                   
    print $ lookup 1 hashMap              -- Just 'a'                                               
    print $ lookup 5 hashMap              -- Nothing                                                  
    print $ hashMap ! 1                   -- 'a'                                                
    print $ lookupDefault 'N' 5 hashMap   -- 'N'                                                     
    print $ insert 4 'd' hashMap          -- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d')]                     
    print $ delete 2 hashMap              -- fromList [(1,'a'),(3,'c')]                                      
    print $ map (toUpper) hashMap         -- fromList [(1,'A'),(2,'B'),(3,'C')]                                
    print $ filter (> 'a') hashMap        -- fromList [(2,'b'),(3,'c')]                                  














