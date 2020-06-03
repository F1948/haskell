module ListComprehensions where


qsort [] = []
--qsort (s:xs) = qsort (filter (< s) xs) ++ [s] ++ qsort (filter (>= s) xs)
qsort (s:xs) = qsort [x | x<-xs, x<s] ++ [s] ++ qsort [x | x<-xs, x>=s]

boomBang xs = [ (if x < 10 then "BOOM!" else "BANG!", show x) | x <- xs, mod x 2 == 0]
bB = boomBang

a = [(a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
b = [(a,b,c) | c <- [1..100], b <- [1..100], a <- [1..100], a^2 + b^2 == c^2]
x = [s | s <- b , s `notElem` a ]

main = putStrLn "Hello, world!"

n = [8, 15, 7, 2, 11, 1, 14, 9, 13, 21, 3]

doubleFact :: Integer -> Integer
doubleFact n = product [n, n-2 .. 1]



coins = [2, 3, 7] :: (Num a, Ord a) => [a]

--change :: (Ord a, Num a) => a -> [[a]]

change 0 = [[]]
change x = [ s:xs | s <- coins, s <= x, xs <- change (x - s)]





 









