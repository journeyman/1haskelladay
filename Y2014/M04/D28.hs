module Y2014.M04.D28 where

--source https://twitter.com/1HaskellADay/status/460674924877590528
--solution https://twitter.com/1HaskellADay/status/460826282297540608

{-- splits
    Lists the possible splits of a list

   Examples
   >>> splits [1..4]
   [([],[1,2,3,4]),([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4]),([1,2,3,4],[])]

   >>> splits []
   [([],[])]

   >>> splits "hello"
   [("","hello"),("h","ello"),("he","llo"),("hel","lo"),("hell","o"),("hello","")]
--}

test :: [(String, String)]
test = splits "hello"

splits :: [a] -> [([a],[a])]
splits xs = scanl (\(sub1, sub2) _ -> (sub1 ++ [head sub2], tail sub2)) ([], xs) xs
