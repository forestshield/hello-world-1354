-- Book Notes 4 Professionals 
module Notes4Pros1 where 

--import Prelude 

--main :: IO () 
--main = do 


-- variant 1
fac :: (Integral a) => a -> a
fac n = product [1..n]

-- variant 2
fac2 :: (Integral a) => a -> a
fac2 0 = 1
fac2 n = n * fac2 (n -1)

-- variant 3 (my own)
fac3 :: (Integral a) => a -> a
fac3 0 = 1
fac3 n = product [(fac3 0)..n]

-- natural sequnce
nat :: (Integral a) => a -> a
nat 0 = 0
nat n = n

-- sum of natural
nat2 :: (Integral a) => a -> a
--nat2 0 = 0
nat2 n = sum [1..n]

-- fibonachi
{-
f (0) <- 0
f (1) <- 1
f (n) <- f (n-1) + f (n-2)
-}
{-
fibs !! n <- f (n)
fibn = fibs !! 
    where
    fibs = 0 : 1 : map f [2..]
    f n = fibs !! (n-1) + fibs !! (n-2)
-}

-- list functions in Prelude
-- :browse Data.Char
-- :browse

--main = print $ fac 5
--main = print $ fac2 8
--main = print $ fac3 10
--main = print $ nat 10
--main = print $ nat2 10

s1 = print $ fac 5
s2 = print $ fac2 8
s3 = print $ fac3 10
s4 = print $ nat 13
s5 = print $ nat2 16

--prep4res :: Integral -> Integral
--prep4res (a:[]) = fac 5 : fac2 8 : fac3 15: nat 10 : nat2 36

--main = print $ fac 5 0 fac2 8 
--prep4res (a:[]) = fac 5 : fac2 : fac3 : nat : nat2

----------------
-- example from intenet footbar.com
{-
factorCount number = factorCount' number isquare 1 0 - (fromEnum $ square == fromIntegral isquare)
    where square = sqrt $ fromIntegral number
          isquare = floor square

factorCount' number sqrt candidate count
    | fromIntegral candidate > sqrt = count
    | number `mod` candidate == 0 = factorCount' number sqrt (candidate + 1) (count + 2)
    | otherwise = factorCount' number sqrt (candidate + 1) count

nextTriangle index triangle
    | factorCount triangle > 1000 = triangle
    | otherwise = nextTriangle (index + 1) (triangle + index + 1)

main = print $ nextTriangle 1 1
-}

-- example 2 from intenet footbar.com
{-
factorCount number = factorCount' number isquare 1 0 - (fromEnum $ square == fromIntegral isquare)
    where square = sqrt $ fromIntegral number
          isquare = floor square

factorCount' :: Int -> Int -> Int -> Int -> Int
factorCount' number sqrt candidate0 count0 = go candidate0 count0
  where
  go candidate count
    | candidate > sqrt = count
    | number `rem` candidate == 0 = go (candidate + 1) (count + 2)
    | otherwise = go (candidate + 1) count

nextTriangle index triangle
    | factorCount triangle > 1000 = triangle
    | otherwise = nextTriangle (index + 1) (triangle + index + 1)

main = print $ nextTriangle 1 1
-}

{-
-- program to copy a file --
import System.Environment

main = do
         --read command-line arguments
         [file1, file2] <- getArgs

         --copy file contents
         str <- readFile file1
         writeFile file2 str
-}

--main print 5 