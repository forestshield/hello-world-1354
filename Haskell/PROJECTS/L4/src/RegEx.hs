-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-regular-expression-matcher

module RegEx where

import GHC.Exts (IsString(..))
--import Prelude hiding (<>)

data Regexp = Zero                  -- empty
            | One                   -- epsilon
            | Lit Char              -- single character
            | Plus Regexp Regexp    -- union (+)
            | Cat  Regexp Regexp    -- concatenation (.)
            | Many Regexp           -- repetition (*)
            deriving Show

infixl 6 <+>
infixl 7 <>

(<+>) :: Regexp -> Regexp -> Regexp
Zero <+> e = e
e <+> Zero = e
e1 <+> e2  = Plus e1 e2

(<>) :: Regexp -> Regexp -> Regexp
Zero <> _   = Zero
_ <> Zero   = Zero
One <> e    = e
e <> One    = e
e1 <> e2    = Cat e1 e2

many :: Regexp -> Regexp 
many Zero     = One
many One       = One
many (Many e)  = Many e
many e         = Many e

type Cont= String -> Bool

accept :: Regexp -> String -> Cont -> Bool  -- worker function
accept Zero    cs      k = False
accept One     cs      k = k cs
accept (Lit c) (c':cs) k = c==c' && k cs
accept (Lit c) []      k = False
accept (Cat e1 e2) cs  k = accept e1 cs (\cs' -> accept e2 cs' k)
accept (Plus e1 e2) cs k = accept e1 cs k || accept e2 cs k
accept (Many e) cs k     = acceptMany e cs k
  where 
     acceptMany e cs k 
       = k cs || accept e cs (\cs' -> cs'/=cs && acceptMany e cs' k)

match :: Regexp -> String -> Bool
match re s = accept re s null

instance IsString Regexp where
  fromString cs = foldr ((RegEx.<>) . Lit) One cs

regExTest = do
  print "(match (\"ab\" RegEx.<> many \"ba\") \"abbaba\")"
  print (match ("ab" RegEx.<> many "ba") "abbaba")
