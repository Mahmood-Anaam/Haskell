module MP3a where

import Data.List
import Data.List.Split


{-
  Binary tree related type definitions.
-}
data BinTree a = Node a (BinTree a) (BinTree a) 
data Direction = L | R 

instance Eq Direction where 
  L == L =  True
  R == R =  True
  L==_   =  False
  R==_   =  False

{-
  Creates a `BinTree` where all nodes contain the specified value.
-}
treeRepeat :: a -> BinTree a
treeRepeat n= Node n (treeRepeat n) (treeRepeat n)


{-
  Creates a `BinTree` where the nodes are populated with the natural numbers,
  starting at the "root" of the tree and then downwards and from left to right
  across each level of the tree.
-}

treeNats :: BinTree Integer
treeNats = createNat 1

createNat :: Integer -> BinTree Integer
createNat n=Node n (createNat (n*2)) (createNat (n*2+1))



{-
  Takes a list of `Direction` values (`L`eft or `R`ight) and traverses the tree
  to return the value in the target node.

  Examples:

  treeVal [L,R] treeNats => 5

  treeVal [] treeNats => 1
-}
treeVal :: [Direction] -> BinTree a -> a
treeVal [] (Node v _ _)=v
treeVal (d:dirs) (Node v left right)=if d==L then treeVal dirs left else treeVal dirs right

                    

{-
  Converts a tree to a list; the root of the tree is the first list value, and
  the values in the tree are taken downwards and across each level.

  Examples:

  take 10 $ treeToList treeNats
  => [1,2,3,4,5,6,7,8,9,10]
-}

treeToList :: BinTree a -> [a]
treeToList tree=tbf [tree]
      where
        tbf xs = mymap nodeValue xs ++ tbf (myconcat (mymap leftAndRightNodes xs))
        nodeValue (Node a _ _) = a
        leftAndRightNodes (Node _ a b)= [a,b]


mymap:: (a->b)->[a]->[b]
mymap f []=[]
mymap f (x:xs)=(f x):mymap f xs

myconcat:: [[a]]->[a]
myconcat []=[]
myconcat (x:xs)=x++myconcat xs


{-
  "Flips" the `BinTree` so that we obtain the mirror image of the original tree.

  For instance, flipping the tree on the left gives us the one on the right:

             1                     1
           /   \                 /   \
          2     3      =>       3     2
         / \   / \             / \   / \
        4   5 6   7           7   6 5   4
-}


treeFlip :: BinTree a -> BinTree a
treeFlip (Node v l r)= Node v (treeFlip r) (treeFlip l) 


{-
  Returns a `BinTree` based on an infinite list where the first item of the list
  is the root, and subsequent items from the list are assigned to nodes
  downwards and across the levels of the tree.

  Examples:

  take 10 $ treeToList $ treeFromList [1..]
  => [1,2,3,4,5,6,7,8,9,10]

  Hint: check out your `treeNats` for inspiration!
-}
treeFromList :: [a] -> BinTree a
treeFromList ns= creatL 0 ns
      where
        creatL index ns=Node (ns!!index) (creatL (2*(index+1)-1) ns) (creatL (2*(index+1)) ns)
          



{-
  Takes a function and an initial value, and returns a `BinTree` where the root
  value is the initial value, and values in subsequent nodes are based on
  repeated applications of the given function to the value.

  Examples:

  treeVal [R,R,R] $ treeIterate (2*) 1
  => 16384

  take 15 $ treeToList $ treeFlip $ treeIterate (2*) 1
  => [1,4,2,64,32,16,8,16384,8192,4096,2048,1024,512,256,128]

  Hint: checkout `iterate`.
-}

treeIterate ::(a -> a) -> a -> BinTree a
treeIterate f n=treeFromList (myiterate f n)

myiterate :: (a -> a) -> a -> [a]
myiterate f a = a : myiterate f (f a)



{-
  BinTree instance of the Functor class.
-}
instance Functor BinTree where
  fmap f (Node v l r)= Node (f v) (fmap f l) (fmap f r)
  


{-
  BinTree instance of the Applicative class.
-}



instance Applicative BinTree where
  -- (<*>) = undefined
  
  pure n = Node n (pure n) (pure n)
  (<*>) (Node vf lf rf) (Node vt lt rt) =Node (vf vt) ((<*>) lf lt) ((<*>) rf rt)