module Perms where

--import Data.List

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concatMap (insertElem x) (perms xs) where
      insertElem x [] = [[x]]
      insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)
--    insertElem 1 [2,3]     -> [1,2,3]: map 2: ([1,3] : map 3: [[1]])
      

perms :: [a] -> [[a]]
perms xs0        =  xs0 : perms1 xs0 []
  where
    perms1 []     _  = []
    perms1 (t:ts) is = foldr interleave (perms1 ts (t:is)) (perms is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
 

perms2 :: (Eq a) => [a] -> [[a]]
perms2 xs = printResult (mainLoop [([],xs)] $ length xs) 
  where
    printResult = map (\(a, b) -> a)
    mainLoop xs n = if n <= 0 then xs else mainLoop (concatMap fx xs) (n-1)
    fx (xs, ys) = map (pairProc xs ys) ys
    pairProc xs ys p = (p:xs, filter (/=p) ys)



{-
-- генератор перестановок (original!)
-- факториал как бонус) run: fact n -> n!
-- run: runFFX [1..7]; testFFX_1 [1..5]; testFFX_2 [1..5]

fx (x, xs) = map (\y -> (y:x, (filter (/=y) xs) )) xs
ffx xs n = if n <= 0 then xs else ffx (concatMap fx xs) (n-1)
ffx_run xs = map (\(a, b) -> a) (ffx [([],xs)] $ length xs)

fact n = product (take n [1..])

testFFX_1 xs = (length (ffx [([],xs)] $ length xs), fact $ length xs)

testFFX_2 xs = (length a, length (nub a)) where a = runFFX xs
-}
