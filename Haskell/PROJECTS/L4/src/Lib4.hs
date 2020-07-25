module Lib4
    ( someFuncLib4
    ) where

someFuncLib4 :: IO ()
someFuncLib4 = do
  print "------- Working with Lists ------"
  putStrLn "putStrLn Lambda: λ someFuncLib4"      -- putStrLn Lambda: λ
  print awesome                                   -- ["Papuchon","curry",":)"]
  --awesome ++ also
  print sList1            -- ["Papuchon","curry",":)","Quake","The Simons"]
  print allAwesome        -- [["Papuchon","curry",":)"],["Quake","The Simons"]]
  print sList2            -- ["Papuchon","curry",":)","Quake","The Simons"], after concat
  print bVal
  putStrLn $ "length allAwesome = " ++ show (length allAwesome) -- 2
  putStrLn $ "length (concat allAwesome) = " ++ show (length (concat allAwesome))   -- 5
  putStrLn ".......isPaliandrome  'abcd' ..............." 
  print (isPalindrome "abcd")
  putStrLn ".......isPaliandrome2  'eabae' ..............." 
  print (isPalindrome2 "eabae")
  putStrLn ".......myAbs (-4) ..............." 
  print (myAbs (-4))
  putStrLn ".......myAbs2 5 ..............." 
  print (myAbs2 5)
  putStrLn ".......simpliest func with Lambda syntaxis .........." 
  print $ id3 "xxx"
  print $ id3 5
  print $ id2 [1,2,4]
  print $ id2 (5, "tuples")


--working with Lists
par1 = "Papuchon"
awesome = [par1, "curry", ":)"]
sL41 = "The Simons"
also = ["Quake", sL41]
allAwesome = [awesome, also]
sList1 = awesome ++ also
sList2 = concat allAwesome  -- changes List from 2 nested Lists to List of 5 strings
bVal = sList1 == sList2

--isPalindrome
isPalindrome :: (Eq a) => [a] -> Bool
--isPalindrome x = undefined
isPalindrome x = do  
  let y = reverse x
  x == y

--isPalindrome2 better one
isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 x = reverse x == x
  
--returning abs value
--myAbs better one
myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else negate x

--myAbs2
myAbs2 :: Integer -> Integer
myAbs2 x = do
  if x > 0 
    then x 
  else 
    negate x

--simpliest function
id3 :: a -> a
id3 x = x

--simpliest function, lambda notation
id2 :: a -> a
id2  = \x -> x

--

