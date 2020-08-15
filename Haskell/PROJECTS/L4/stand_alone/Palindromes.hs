main = interact respondPalindromes

{-
func15main = interact shortLinesOnly  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result    

--- even shorter version, using "interact"
func16main = interact $ unlines . filter ((<10) . length) . lines 

--- is Panlindrome function 
respondPalindromes' contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))  
    where   isPalindrome xs = xs == reverse xs
-}

--- pont-free version is Panlindrome function 
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs




















