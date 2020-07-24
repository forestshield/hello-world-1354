-- first prog

main :: IO()
main = do
    print "simple func"
{-    
    print "Who is email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "who is the Author?"
    author <- getLine

    print recipient
    print title
    print author

    print ("Dear " ++ recipient ++ ", \n" ++ 
        "Thanks for buying " ++ title ++ "\nthanks, \n" ++
        author )

    print (createEmail recipient title author)

-- helper functions
toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"
fromPart author = "Thanks,\n"++author
createEmail recipient bookTitle author = toPart recipient ++
                                          bodyPart bookTitle ++
                                          fromPart author
-}

simple x = x
