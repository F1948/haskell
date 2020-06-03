-- {-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}

module Stp where

import Data.Char
import Data.List
import Data.Function


data Boo = Tru | Fls
  deriving (Show)
class TmpClass a where
  (-+) :: a -> a -> Boo
instance TmpClass Boo where
  Fls -+ Fls = Fls
  _   -+ _   = Tru


class Printable a where
  toString :: a -> String
instance Printable Bool where
  toString True = "true"
  toString False = "false"
instance Printable () where
  toString () = "unit type"
instance (Printable a, Printable b) => Printable (a,b) where
  toString (p1, p2) = "(" ++ toString p1 ++ ", " ++ toString p2 ++ ")"


class (Enum a, Bounded a, Ord a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x < maxBound = succ x
          | otherwise = minBound

  spred :: a -> a
  spred x | x > minBound = pred x
          | otherwise = maxBound
          
instance SafeEnum Bool


fromInt2Num :: Int -> Double
fromInt2Num x = fromIntegral x  / 2

fibonacci' :: Integer -> Integer
fibonacci' n | n == 0    = 0
            | n == 1    = 1
            | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0  = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci :: Integer -> Integer
fibonacci = f 0 1 where
  f a b n | n == 0  = a
          | n > 0   = f b (a+b) (n-1)
          | n < 0   = f b (a-b) (n+1)
        
fib = fibonacci
fib' = fibonacci'
--  1 2 3 4 5 6 7  8  9  10
-- +1 1 2 3 5 8 13 21 34 55


seqA :: Integer -> Integer
seqA = f 1 2 3 where
  f a b c n |n == 0 = 1
            |n == 1 = b
            | otherwise = f b c (b + c - 2*a) (n-1)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sum a, b) where 
  a = map read [ [n] | n <- show x, isDigit n] :: [Integer]
  b = sum $ take (length a) [1, 1..] :: Integer


-- интегралл методои трапеции:
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = step * ( (f a + f b) / 2 + (summ f (a+step) 0 n) )
  where summ f a x n | n <= 1 = x
                     | otherwise = summ f (a+step) (x + f a) (n-1)
        step = (b-a)/n
        stop = b - step
        n = 1000


integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b = step * ( (f a + f b) / 2 + sum (map f summ) )
  where summ = [ x | x <- [a+step, a+2*step .. b-step] ]
        step = (b-a)/n  
        n = 1000

int = integration
int' = integration'

int2 a b c = (int a b c, int' a b c)

on' = (+) `on` (fst) -- применить "+" к результатам "^2"


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x:xs) (y:ys) []     = x+y : sum3 xs ys []
sum3 (x:xs) []     (z:zs) = x+z : sum3 xs [] zs
sum3 []     (y:ys) (z:zs) = y+z : sum3 [] ys zs
sum3 (x:xs) []     []     = x : sum3 xs [] []
sum3 []     (y:ys) []     = y : sum3 [] ys []
sum3 []     []     (z:zs) = z : sum3 [] [] zs
sum3 []     []     []     = []
sum3 (x:xs) (y:ys) (z:zs) = x+y+z : sum3 xs ys zs

sum3' :: Num a => [a] -> [a] -> [a] -> [a]
sum3' [] [] [] = []
sum3' [] ys zs = sum3 [0] ys zs
sum3' xs [] zs = sum3 xs [0] zs
sum3' xs ys [] = sum3 xs ys [0]
sum3' (x:xs) (y:ys) (z:zs) = x+y+z : sum3 xs ys zs

{-instance (Num a) => Num [a] where
  (+) [] [] = []
  (+) xs [] = xs
  (+) [] xs = xs
  (+) (x:xs) (y:ys) = (x+y):(xs + ys)
sum3x :: Num a => [a] -> [a] -> [a] -> [a]
sum3x a b c = a + b + c -}

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = (x: takeWhile (==x) xs) : groupElems (dropWhile (==x) xs)


readDigits :: String -> (String, String)
readDigits = span isDigit 


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> or [p1 x, p2 x])


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])


