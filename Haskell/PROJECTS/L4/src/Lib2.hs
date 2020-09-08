module Lib2
    ( someFunc5
    ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Char

-- we put these to packagy.yaml
--{-# LANGUAGE UnicodeSyntax #-}
--{-# LANGUAGE OverloadedStrings #-}

aWord0 :: T.Text
aWord1 :: T.Text
aWord2 :: T.Text
bgText :: T.Text
dharma :: T.Text
lambdT :: T.Text
aWord0 = "Cheese"
aWord1 = "Хорошего нам всем века, года, месяца, дня, жизни! :)"
aWord2 = "哈斯克尔7.6.1"
dharma = "धर्म"
bgText = " श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो"
lambdT = "TIO.putStrLn Lambda: λ"

someFunc5 :: IO ()
someFunc5 = do
  putStrLn "================================= Lib2 ==================================="

  print aWord1                      -- "\1061\1086\1088\1086\1096\1077\1075\1086 ...
  print aWord2                      -- "\21704\26031\20811\23572\&7.6.1"
  print aWord0                      -- "Cheese"
  print 'a'                         -- 'a'
  print 'λ'                         -- '\955'

  putStrLn "abc"                    -- abc
  putStrLn ['^', '$', '&', 'λ', '>', 'a']   -- ^$&λ>a
  TIO.putStrLn lambdT                       -- TIO.putStrLn Lambda: λ
  putStrLn "putStrLn Lambda: λ"             -- putStrLn Lambda: λ

  TIO.putStrLn aWord1   -- Хорошего нам всем века, года, месяца, дня, жизни! :)
  TIO.putStrLn aWord2   -- 哈斯克尔7.6.1
  TIO.putStrLn aWord0   -- Cheese
  TIO.putStrLn dharma   -- धर्म
  TIO.putStrLn bgText   -- श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो
  TIO.putStrLn $ highlight dharma bgText -- श्रेयान्स्व{धर्म}ो विगुणः पर{धर्म}ात्स्वनुष्ठितात्।स्व{धर्म}े निधनं श्रेयः पर{धर्म}ो

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
    where pieces = T.splitOn query fullText
          highlighted = mconcat ["{",query,"}"]
