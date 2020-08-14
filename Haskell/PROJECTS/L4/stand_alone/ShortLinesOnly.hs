-- Let's make program that takes some input and prints out only those lines that are shorter 
--      than 10 characters.
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
---  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
-----
-- 12345678\n1234567890\n12\nqwertyuiop\n



















