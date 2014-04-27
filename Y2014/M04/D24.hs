module Y2014.M04.D24 where

--source https://twitter.com/1HaskellADay/status/459225369619353600
--solution https://twitter.com/1HaskellADay/status/459375311357214722
import Data.Tree


-- Find an implementation of
dumbledore :: (a -> [b] -> b) -> Tree a -> b
dumbledore f (Node x []) = f x []
dumbledore f (Node x xs) = f x (map (dumbledore f) xs)


{- | use dumbledore to compute the height of a tree

   Example:

   >>> heigth $ Node 4 [Node 5 [], Node 2 [Node 3 []]]
   3

-}

tree1 :: Tree Int
tree1 = Node 4 [Node 5 [], Node 2 [Node 3 []]]

test :: String
test = show $ height tree1

height :: Tree a -> Int
height = dumbledore (\_ ms -> 1 + mymax ms)

mymax :: [Int] -> Int
mymax [] = 0
mymax xs = maximum xs

height' :: Tree a -> Int
height' (Node _ []) = 1
height' (Node _ xs) = 1 + maximum (map height xs)