module Y2014.M04.D29 where

--source https://twitter.com/1HaskellADay/status/461037308108500992
--solution https://twitter.com/1HaskellADay/status/461188677012434945


{- | Act like takeWhile but includes the element that fails the test

   >>> takeWhileWithFailure odd [1..4]
   [1,2]

   >>> takeWhileWithFailure odd [1,3,5]
   [1,3,5]

   >>> takeWhileWithFailure ((2 ==) . length)) ["hi", "world", "world", "world"]
   ["hi","world"]

   >>> takeWhileWithFailure (<5) [10..20]
   [10]
-}


test :: [Integer]
test = takeWhileWithFailure (<5) [10..20]

takeWhileWithFailure :: (a -> Bool) -> [a] -> [a]
takeWhileWithFailure _ [] = []
takeWhileWithFailure f (x:xs) = if f x 
								then x : takeWhileWithFailure f xs 
								else [x]

theirs :: (a -> Bool) -> [a] -> [a]
theirs = ((uncurry (++) . fmap (take 1)) .) . span