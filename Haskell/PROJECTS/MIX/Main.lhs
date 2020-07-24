This is a sample .lhs file

\begin{code}% this is not really code
module Main where

addition a b = a + b
\end{code}

Here is just a text

\begin{code}
main :: IO ()
main = do let z = addition 5 5
          putStrLn $ "The result is: " ++ show z
\end{code}

See, if could compile it