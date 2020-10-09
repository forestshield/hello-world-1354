{-# OPTIONS_GHC -fwarn-missing-signatures #-}
module GHCOptions where  


funcGHCOptions1 :: IO ()  -- if this is missing, with this line, we are getting this warning 
                          --    warning: [-Wmissing-signatures]
                          --    Top-level binding with no type signature: funcGHCOptions1 :: IO ()
funcGHCOptions1 = do  
  putStrLn "Using GHC Options, {-# OPTIONS_GHC -fwarn-missing-signatures #-}"
  putStrLn "Hello, world!"
  funcWithMissingDeclaration
  putStrLn "\n"

--funcWithMissingDeclaration :: IO ()
funcWithMissingDeclaration = putStrLn "function With Missing Declaration produces something\
  \\n, like this:  warning: [-Wmissing-signatures]\nTop-level binding with no type signature:\
  \funcWithMissingDeclaration :: IO ()"
