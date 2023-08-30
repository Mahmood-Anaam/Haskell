module MP3b where

{-
  Playing card definitions (feel free to add your own supporting types, so long 
  as you keep `Card`).
-}
data Card = Card{ rank :: Rank, suit :: Suit } deriving (Show,Eq)

data Suit = 
      Spades 
    | Hearts 
    | Clubs 
    | Diamonds
    deriving (Show, Eq, Enum,Ord,Bounded)

data Rank = 
      Two
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
    deriving (Show, Eq, Ord, Enum, Bounded)



{-
  A full deck of 52 playing cards.
-}

deck :: [Card]
deck = [ Card rank suit
          | suit <- [Hearts, Diamonds, Clubs, Spades]
          , rank <- [Two .. Ace] ]

{-
  Hand types. Don't change these.
-}
data Hand = HighCard  | Pair | TwoPair | ThreeOfAKind | Straight
            | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
            deriving (Eq, Show, Ord)


{-
  Takes a list of 5 cards and returns the strongest hand they can be
  used to make. 

  Examples (note that your `Card` values may look different):

  hand [Card 2 H, Card 3 D, Card Ace H, Card 5 D, Card 4 S]
  => Straight

  hand [Card 2 D, Card 3 C, Card 2 C, Card 3 D, Card 2 H]
  => FullHouse
-}
hand :: [Card] -> Hand
hand cards= if sameSuits cards then checksameSuits cards else checknotsameSuits cards



checksameSuits:: [Card] -> Hand
checksameSuits cards
         |ras==[Ace,King,Queen,Jack,Ten]         = RoyalFlush
         |issquenst ras                          = StraightFlush
         |otherwise                              = Flush        
          where
            ras=sortdecs [rank c|c<-cards]


checknotsameSuits:: [Card] -> Hand
checknotsameSuits cards
         |length leng==2 &&  maximum leng ==4              = FourOfAKind
         |length leng==2 &&  maximum leng ==3              = FullHouse
         |issquenst ras                                    = Straight
         |length leng==3 &&  maximum leng ==3              = ThreeOfAKind
         |length leng==3 &&  maximum leng ==2              = TwoPair
         |length leng==4 &&  maximum leng ==2              = Pair
         |otherwise                                        = HighCard        
          where
            ras=sortdecs [rank c|c<-cards]
            rsag=mygroup ras
            leng=[length g |g<-rsag]

         


issquenst::[Rank]->Bool
issquenst (f:ras)=difran ras && (f==Ace && (last ras==Two))
                where difran (r:rass)
                       | null rass                       =True
                       | (fromEnum r-fromEnum (rass!!0)) ==1 =difran rass
                       | otherwise                           =False


sameSuits::[Card] ->Bool
sameSuits cards=all (== head sus) (tail sus)
       where
        sus=[suit c|c<-cards]

-- sortranks::[Card] ->[Rank]
-- sortranks cards=reverse $ sort [rank c|c<-cards]



{-
  Takes a list of 5-`Card` lists, and returns a list of tuples of type 
  `(Int, Hand)`, where each tuple indicates the number of times a certain 
  `Hand` occurs in the input list. The tuples should be sorted in decreasing 
  order of frequency.
  
  See the machine problem write-up on how to test this function with the 
  generators defined for you below.
-}
computeStats :: [[Card]] -> [(Int, Hand)]

computeStats cs= sortdecs $ grouping $ mygroup $ mymap hand cs
           where 
            grouping []=[]
            grouping (g:hgs)=(length g,g!!0):grouping hgs




mymap:: (a->b)->[a]->[b]
mymap f []=[]
mymap f (x:xs)=(f x):mymap f xs



sortdecs::Ord a => [a] -> [a]
sortdecs [] = []
sortdecs (x:xs) = insert x (sortdecs xs)
     where 
      insert z [] = [z]
      insert z (y:ys)
         | z > y     = z:y:ys
         | otherwise = y:insert z ys


mygroup::Ord a => [a] -> [[a]]
mygroup [] = []
mygroup (x:xs) = insert x xs : mygroup (del x xs)
    where 
      insert z [] = [z]
      insert z (y:ys)
        | z == y     = y:insert z ys
        | otherwise =  insert z ys
      
      del z []=[]
      del d (dx:dxs)
        | d == dx     = del d dxs
        | otherwise =  dx:del d dxs

