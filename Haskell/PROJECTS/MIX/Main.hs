module Main where

addition a b = a + b

main :: IO ()
main = do let z = addition 5 5
  putStrLn $ "The result is: " ++ show z
