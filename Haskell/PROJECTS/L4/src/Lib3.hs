module Lib3
    ( someFunc6
    ) where

import FunctionWithWhere
import FunctionWithLet
import TopLevelFunction
import Print3flipped

someFunc6 :: IO ()
--someFunc6 = putStrLn "someFunc"
--someFunc6 = sayHello "someFunc"
--someFunc6 = putStrLn (strResult)
--someFunc6 = putStrLn (show quot1)
--someFunc6 = putStrLn (show resSec2'')
--someFunc6 = printInc2 6
--someFunc6 = putStrLn (show mult1)
--someFunc6 = putStrLn (show mult4)
--someFunc6 = putStrLn (show resExp2)
--someFunc6 = putStrLn (show resExp2)
{-
str = concat [show waxOn, " ", show resExp2]
str2 = str ++ "\n"
someFunc6 = putStr str2
-}
--someFunc6 = putStrLn (show $ topLevelFunction 8)
--someFunc6 = funcLikeMain
someFunc6 = do
    main            -- this is a different main from Print3flipped (not a nice style, just an experiment)
    funcLikeMain

--1
sayHello :: String -> IO ()
sayHello x = 
    putStrLn ("Hello, " ++ x ++ "!")

--2
x = 10 * 5 + y
myResult = x * 5
y = 10
strResult :: String
strResult = show myResult
--3
area x = pi * (x * x)

--4
double x = x * 2

--5
x1 = 7
y1 = 10
f = x1 + y1

-- basic expressions and functions
a = 1 + 1               -- 2
b = 1 - 1               -- 0
c = 1 * 1               -- 1
c1 = 1 * 1.0            -- 1.0
d = 1 / 1               -- 1.0
div' = div 1 1          -- 1
mod' = mod 1 1          -- 0
quot' = quot 1 1        -- 1
rem' = rem 1 1          -- 0

-- rounds down
div0 = div 20 6         -- 3
div1 = div 20 (-6)      -- -4
div2 = div (-20) (-6)   -- 3
div3 = div (-20) 6      -- -4
div4 = div 6 20         -- 0
div5 = div 6 (-20)      -- -1
div6 = div (-6) 20      -- -1
div7 = div (-6) (-20)   -- 0

-- rounds toward zero
quot0 = quot 20 6       -- 3
quot1 = quot 20 (-6)    -- -3
quot2 = quot (-20) 6    -- -3
quot3 = quot (-20) (-6) -- 3
quot4 = quot 6 20       -- 0
quot5 = quot 6 (-20)    -- 0
quot6 = quot (-6) 20    -- 0
quot7 = quot (-6) (-20) -- 0

----------------------------------------
-- laws for quotients and remainders
lawOne = quot x y * y + rem x y == x
funcLawOne x y = quot x y * y + rem x y
--
lawTwo = div x y * y + mod x y == x
funcLawTwo x y = div x y * y + mod x y

--
mod0 = mod 15 12        -- 3
mod1 = mod 12 15        -- 12
mod2 = mod (-12) (-15)  -- -12
mod3 = mod (-15) (-12)  -- -3
mod4 = mod 12 (-15)   -- -3
mod5 = mod 15 (-12)   -- -9
mod6 = mod (-12) 15   -- 3
mod7 = mod (-15) 12   -- 9

--
rem0 = rem 15 12        -- 3
rem1 = rem 12 15        -- 12
rem2 = rem (-12) (-15)  -- -12
rem3 = rem (-15) (-12)  -- -3
rem4 = rem 12 (-15)     -- 12
rem5 = rem 15 (-12)     -- 3
rem6 = rem (-12) 15   -- -12
rem7 = rem (-15) 12   -- -3

--------------------
-- What day of the week will be in 23 days if today was a Monday (1)
-- Sunday (0), Tuesday (2) and so on    
day23 = mod (1 + 23)  7 -- 3 (Wednsday)
day23' = rem (1 + 23)  7 -- 3 (Wednsday)

-- day 5 from Saturday
day5 = mod (6 + 5) 7    -- 4 (Thursday)
day5' = rem (6 + 5) 7    -- 4 (Thursday)

-- day 12 days ago, if today was Wednesday (3)
day12ago = mod (3 - 12) 7   -- Friday (5)
day12ago' = rem (3 - 12) 7  -- -2

-- results of differnt signs of divisor and divident
res0 = (-5) `mod` 2         -- 1
res1 = 5 `mod` (-2)         -- -1 
res2 = (-5) `mod` (-2)      -- -1
res3 = (-5) `rem` 2         -- -1
res4 = 5 `rem` (-2)         -- 1
res5 = (-5) `rem` (-2)      -- -1
 
 -- negate
resNeg = 2000 + negate 1234  -- 766

-- $
result0 = 2 ^ (2 + 2)          -- 16
result1 = (2^) $ 2 + 2          -- 16
result2 = 2 ^ 2 + 2            -- 6
result3 = (2^) $ (+2) $ 3*2     -- 256
--result4 = (2^) $ 2 + 2 $ (*30)  -- compiler error
result4 = (2^) $ (*30) $ 2 + 2  -- 1329227995784915872903807060280344576
result5 = (2^) $ (*30) 4        -- 1329227995784915872903807060280344576
result6 = (2 ^) 120              -- 1329227995784915872903807060280344576
result7 = (2 ^) 120               -- 1329227995784915872903807060280344576

-- sectioning (*30) == function (*30)
resSec0 = (*30) 4                -- 120
resSec1 = (+30) 4                -- 34

--resSec2 = (-30) 4              -- does not compile
resSec2 = (-) 30 4               -- 26
resSec2' = (30 -) 4              -- 26
resSec2'' = (subtract 4) 30      -- 26

resSec3 = (/30) 4                -- 0.13333333333333333
resSec4 = (30/) 4                -- 7.5

-- let expressions
--let x2 = 5 in x2               -- works in ghci only 
-- changing let to where in code

mult1 = x2 * y2                  -- let x2 = 5; y2 = 6 in x2 * y2
    where x2 = 5
          y2 = 6 

mult2 = x2 * 3 + y2              -- let x2 = 3; y2 = 1000 in x2 * 3 + y2
    where x2 = 3
          y2 = 1000

mult3 = x * 6                    -- here is different x and y
    where y = 12                 -- let y = 12; x = 12 * 5 + y in x * 6
          x = 12 * 5 + y

mult4 = z / x + y                -- let y = negate x; z = y * 10; x = 8 in z / x + y
    where y = negate x
          z = y * 10
          x = 8

-- (^) 10 $ 1 + 1
resExp0 = 2 + 2 * 3 -3           -- 5
resExp1 = (^) 10 $ 1 + 1         -- 100    
resExp2 = 2 ^ 2 * 4 ^ 5 + 1      -- 4097 

waxOn = x * 5                    -- 1125
    where z = 7
          y = z + 8
          x = y ^ 2

-- concat
concat0' = concat [[1.0, 2.1], [3, 4, 5], [6, 7]]  -- convert all to  Fractional
concat0'' = concat [[8, 2], [3, 4, 5], [6.0, 9]]   -- convert all to Fractional
concat1' = concat [[1, 2], [3, 4, 5], [6, 7]]
str2 = [ '\955' , '\956' , '\957' ]
concat2' = concat ["Iowa", "Melman", "Django\x03BB ", str2]
concat3' = concat [['a','m', 'n'] , [ '\x03BB' , '\x03B0' , '\x03AB' ]]
concat3'' = ['a', 'm', 'n'] ++ ['\x03BB', '\x03B0', '\x03AB']