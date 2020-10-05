module ExTT
    ( testExceptionType
--    , pathFileOne
--    , pathFileTwo
    ) where

import Control.Exception
import Data.Typeable (typeOf)

testExceptionType :: IO () -> IO ()
testExceptionType thunk =  catch thunk handler
  where
    -- Catch All Exceptions -- It is not recommended in real life.
    handler :: SomeException -> IO ()
    handler (SomeException e) = putStrLn $ "I caught an exception.\nMessage =  \
    \" ++ show e ++ "\nType of exception = " ++ show (typeOf e)

testFunc1 :: IO ()
testFunc1 = do
  testExceptionType (print $ div 10 0)
  print "---"
  testExceptionType (error "Fatal kernel error")
  print "---"
  testExceptionType (readFile "/etc/shadow" >>= putStrLn)
  print "---"
  testExceptionType (print $ head ([] :: [Int]))
  print "---"
  --testExceptionType (print $ head [])
  --testExceptionType (head [])
  testExceptionType (print $ head [1, 2, 3])
  print "---"
  --testExceptionType (putStrLn "Insert a line" >> getLine >>= putStrLn)
  