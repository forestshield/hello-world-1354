-- N.B. !!! To print properly Unocode strings on Windows PowerShell and avoid console eception
-- "*** Exception: <stdout>: hPutChar: invalid argument (invalid character)"
-- type "chcp 65001" in powershell before using ghci !!!
-- use ConEmu on Windows, "https://www.fosshub.com/ConEmu.html"

module Main where

import System.IO.CodePage

import Lib
import Lib2
import Lib3
import FunctionWithWhere
import FunctionWithLet
import TopLevelFunction
import Lib4
import Lib5



--import Print3flipped    -- This will not compile, because of another "main" in Print3flipped.hs

--dtM = Woot :: Mood

strA = addZero "a"
strB = addZero "2a"
strC = addZero "2a1b"

main :: IO ()
--main = someFunc         -- works
--main = printInc 14      -- works
--main = printInc2 16     -- works

main = do
  withCP65001 $ withCodePageOptions defaultOptions{chatty = True} cp65001 codePageFunc

codePageFunc :: IO ()
codePageFunc = do 
  someFunc
  someFunc2
  someFunc3
  someFunc4
  funcChekIfCool "I am Cool"
  funcChekIfCool "I am Not Cool"
  putStrLn ".........."
  funcTuples
  showResChMood
  putStrLn "..... addZero for 'a'  '2a' '2a1b' ....."
  print strA
  print strB
  print strC
  putStrLn "..... Int bounds ....."
  printIntBounds
  putStrLn "..... Unicode strings ....."
  printUnicodeStrings
  putStrLn "..... Unicode strings with Data.Text ....."
  someFunc5
  putStrLn "............................" 
  someFunc6
  putStrLn "............................" 
  printInc 2
  --putStrLn "............................" 
  printInc2 4
  putStrLn "............................" 
  someFuncLib4
  someFuncLib5
  putStrLn "............................"
  --someFunc
    