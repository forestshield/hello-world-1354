-- Книга Холмьев. 
-- 
{-
comments 
-}
-- a = 5


{-
-------------------
-- noth1 :: Bool -> Bool   -- this can be uncommented
noth1 a = result where 
    result = not a

a :: Bool
a = False

-- b :: Bool -- this can be uncommented
b = noth1 a
-}
---------------
{- not compiling
reverse :: [a] -> [a]
data Bool = True | False
data [c] = [] | c : [c]
-}

{-
data Date = Date Year Month Day

data Year = Year Int
data Month = January | February | March | April
    | May | June | July | August | September 
    | October | November | December    
data Day = Day Int    
data Week = Monday | Tuesday | Wednesday | Thursday
    | Friday | Saturday | Sunday

data Time = Time Hour Minute Second    
data Hour = Hour Int
data Minute = Minute Int
data Second = Second Int
-}
{-}
--------------------------------
data Bool = True | False
true :: Bool
true = True
false :: Bool
false = False
-}


addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as     = as


func1 :: Float -> Float
func1 f = f + 8.9