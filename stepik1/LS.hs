module ListComprehensions where
import Data.List

-- s = summa :: Int
-- c = coins :: [a]
-- r = result :: []

coins = [2,3,7]

change :: (Ord a, Num a) => [a] -> [a] -> a -> [[a]]
change r _ 0 = [r]
change r c s = concatMap (\x -> change (x:r) [t | t <- c, t <= (s-x)] (s-x)) c

f2 :: [a] -> Int
f2 = foldr (\_ -> (1+)) 0

f1 [] = 0
f1 (s:xs) = 1 + f1 xs


ml :: [Double] -> Double
--ml xs =  (\x -> x / (fromIntegral . length $ xs)) . foldr (+) 0 $ xs
ml = uncurry (/) . foldr (\ x (a,b) -> (x+a, b+1)) (0,0)

m1 :: [a] -> [a]
m1 = reverse . fst . foldl' (\ ~(a, b) x -> ((if odd b then x:a else a), b+1)) ([], 0)

m2 :: [a] -> [a]
m2 = foldl' (\ a x -> x:a) []


