-- Print3flipped.hs
module Print3flipped where 

import Data.Char (isSpace)

myGreetings :: String
myGreetings = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"


main :: IO ()
main = do
  putStr " ... "
  putStrLn myGreetings
  putStrLn secondGreetings
    where secondGreetings = (++) hello ((++) " <from main> " world)
          -- could be secondGreetings = hello ++ " " world


funcLikeMain :: IO ()
funcLikeMain = do
  --putStrLn $ show blank
  putStr [funcRet3rd "one.big.string.here"]
  putStr " λλλ "
  putStrLn myGreetings
  putStrLn secondGreetings
      where secondGreetings = (++) hello ((++) " <funcLikeMain> " world)
          -- could be secondGreetings = hello ++ " " world

funcRet3rd :: String -> Char
funcRet3rd str = ch where
    order = 3
    s = take order str
    a = length str
    ch = str !! (order - 1)

{-
-- does not compile here (it did in Learn1 project)
--funcIsSpace
-- is string blank ? 
-- taken from https://programming-idioms.org/search/is+empty
-- needs also import Data.Char (isSpace)
funcIsSpace :: IO ()
funcIsSpace = do
  let strIn = ['e', 'e']
      blank :: Bool
      blank = all isSpace strIn
-}
-- string operations
someStr0 = head world              -- "w"
someStr0' = head ""                -- Exception empty list
someStr1 = tail world              -- "orld!"
someStr1' = tail ""                -- Exception empty list
someStr2 = take 1 world            -- "w"
someStr3 = take 0 world            -- ""
someStr3' = take (-1) world        -- ""
someStr4 = take 3 world           -- "wor"
someStr4' = take 5 world           -- "world"
someStr4'' = take 15 world           -- "world!"
someStr5 = head $ tail world       -- "o"
someStr6 = head $ tail $ tail world -- "r"
someStr7 = drop 4 world            -- "d!"
someStr8 = drop 100 world          -- ""
someStr9 = drop 1 world            -- "orld!"
someStr10 = drop 0 world           -- "world!"
someStr11 = drop (-1) world        -- "world!"

-- index operator
someChar0 = world !! 0             -- 'w'
someChar1 = world !! 1             -- 'o'
someChar2 = world !! 3             -- 'l'
someChar3 = world !! 77            -- Exception index too large
someChar4 = world !! (-1)          -- Exception negative index
someChar5 = ""    !! (2)           -- Exception index too large
someChar6 = ""    !! (0)           -- Exception index too large

-- some concat, !! and ++ experiments 
res0 = concat [[1, 2, 3], [4, (-5), 6]] -- [1,2,3,4,-5,6]
res1 = (++) [1.0,2,3] [4,5,6]       -- [1.0,2.0,3.0,4.0,5.0,6.0]
res2 = (++) "hello" " world"        -- "hello world!"
res3 = (!!) "hello" 4               -- 'o'
res4 = (take 3 "Julia") ++ (tail "yes")     -- 'Jules'
res5 = 10 * head [1, 2, 3]          -- 10
res6 = "rain" ++ drop 2 "elbow"     -- rainbow
res7 = concat[[1 * 6] , [2 * 6] , [3 * 6]]  -- [6, 12, 18]
res8 = concat [tail [1, 2, 3], 
               tail [4, 5, 6], 
               tail [7, 8, 9]]      -- [2,3,5,6,8,9]        
