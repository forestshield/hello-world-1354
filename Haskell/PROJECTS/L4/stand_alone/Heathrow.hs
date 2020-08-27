import Data.List

-- ================================== Heathrow to London ===============================
--    A --50-- A1-- 5-- A2--40--A3--10--A4 
--             |        |       |       |
--             30       20      25      0
--             |        |       |       |
--    B --10-- B1--90-- B2--2 --B3--8 --B4 
--
--      A node is either a normal node and has information about the road that leads to the 
--      other main road and the road that leads to the next node or an end node, which only 
--      has information about the road to the other main road. A road keeps information about 
--      how long it is and which node it points to. For instance, the first part of the road 
--      on the A main road would be Road 50 a1 where a1 would be a node Node x y, where x and 
--      y are roads that point to B1 and A2
data Node = Node Road Road | EndNode Road
data Road = Road Int Node

--      Another way would be to use Maybe for the road parts that point forward. Each node has 
--      a road part that point to the opposite road, but only those nodes that aren't the end 
--      ones have road parts that point forward
--data Node = Node Road (Maybe Road)  
--data Road = Road Int Node
---
-- sections 50,10,30,  5,90,20,  40,2,25, and 10,8,0
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]
---
heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
---
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]
---
--      Our function, we'll call it optimalPath should thus have a type declaration of 
--      optimalPath :: RoadSystem -> Path. If called with the road system heathrowToLondon, 
--      it should return the following path:
--      [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)] 
---
--      Hint: it will be useful because (Path, Path) -> Section -> (Path, Path) can be used as 
--      the binary function for a left fold, which has to have a type of a -> b -> a
---
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)
---
rsHL1 = roadStep ([], []) (head heathrowToLondon)  -- ([(C,30),(B,10)],[(B,10)])
---
--      Optimization tip: when we do priceA = sum $ map snd pathA, we're calculating the price 
--      from the path on every step. We wouldn't have to do that if we implemented roadStep as 
--      a (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int) function where the 
--      integers represent the best price on A and B.
---
optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath
---
rsHL2 = optimalPath heathrowToLondon  -- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
---
groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)
---
rsHL3 = groupsOf 3 [1..10]            -- [[1,2,3],[4,5,6],[7,8,9],[10]]
---
  
main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice
---
-- file path.txt --
-- 50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0\n
-- cat paths.txt | runhaskell heathrow.hs  
-- The best path to take is: BCACBBC  
-- The price is: 75
