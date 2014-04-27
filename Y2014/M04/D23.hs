module Y2014.M04.D23 where

--source https://twitter.com/1HaskellADay/status/458905718771294208
--solution https://twitter.com/1HaskellADay/status/459014451891421184
import Data.Tree

{- | biggestProduct
   Returns the biggest product of all the paths of the tree

   Examples:
   >>> biggestProduct $ Node 1 [Node 2 [], Node  3 []]
   3

   >>> biggestProduct $ Node 1 [Node 8 [], Node  4 [Node 3 []]]
   12
-}

tree1 :: Tree Int
tree1 = Node 1 [Node 8 [], Node  4 [Node 3 []]]

test :: String
test = show $ biggestProduct tree1

biggestProduct :: (Ord a, Num a) => Tree a -> a
biggestProduct (Node x []) = x
biggestProduct (Node x xs) = (x *) . maximum $ map biggestProduct xs
