import Prelude hiding (concat)

holes :: Char -> Int
holes '0' = 1
holes '1' = 0
holes '2' = 0
holes '3' = 0
holes '4' = 1
holes '5' = 0
holes '6' = 1
holes '7' = 0
holes '8' = 2
holes '9' = 1

holey :: String -> String
holey s = filter p s where
  p :: Char -> Bool
  p c = holes c > 0

chartoInt :: Char -> Int
chartoInt '0' = 0
chartoInt '1' = 1
chartoInt '2' = 2
chartoInt '3' = 3
chartoInt '4' = 4
chartoInt '5' = 5
chartoInt '6' = 6
chartoInt '7' = 7
chartoInt '8' = 8
chartoInt '9' = 9

holesum :: String -> Int

holesum s = sum(map chartoInt(holey s))

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = foldr (++) [] (x:xs)

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
--interleave [] [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys
