This is a sample .lhs file

> module Main where

> addition a b = a + b

Here is just a text

> main :: IO ()
> main = do let z = addition 5 5
>           putStrLn $ "The result is: " ++ show z

See, if could compile it