module Cards (Card, diamond, spade, heart, club) where

--Playing cards
data Suit = Clubs | Diamonds | Hearts | Spades deriving Show 
data Color = Red | Black deriving (Show, Read) 
data Value = Two
           | Three
           | Four
           | Five
           | Six 
           | Seven 
           | Eight
           | Nine
           | Ten 
           | Jack 
           | Queen
           | King
           | Ace 
           deriving (Eq, Ord, Show, Read)
-- Card
data Card = Card
  { suit  :: Suit
  , color :: Color 
  , value :: Value 
  } deriving Show

-- Card2
data Card2 = Card2 Suit Color Value deriving Show

--examples
queenDiamonds :: Card 
queenDiamonds = Card Diamonds Red Queen 
--alternatively
queenHearts :: Card 
queenHearts = Card { suit = Hearts, color = Red, value = Queen }

diamond :: Value -> Card 
diamond = Card Diamonds Red

spade :: Value -> Card 
spade = Card Spades Black

heart :: Value -> Card 
heart = Card Hearts Red

club :: Value -> Card 
club = Card Clubs Black

-- 
data List a = Nil | List a (List a) deriving (Show, Read)
--alternatively
--data List a = Nil | a :+: (List a) deriving (Show, Read)

list :: List Integer
list = List 1 (List 2 (List 3 Nil))


