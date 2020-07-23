module Stp2_12 where

--1
import Control.Applicative (  ZipList(ZipList)
                            , getZipList
                            , (<**>) -- for --4
                            )
-- getZipList $ (\a b c -> 2*a+3*b+5*c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s
--infixl 4 >$<
(>$<) f x = getZipList $ f <$> ZipList x
--infixl 4 >*<
(>*<) fs xs = getZipList $ ZipList fs <*> ZipList xs
x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

strangeDivider a b c = ("/a", ((/) a)) <*> (("/b", ((/) b)) <*> ("/c", c))
--[a b c] -> ( a / ( b / c)


--2
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = ((/) <$> (show2 x, x)) <*> (divideList' xs) where
  show2 a = "<-"++ show a ++ "/"


--3
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
--fmap :: (a -> b) -> (e1 -> e2 -> a) -> (e1 -> e2 -> b)
  fmap f arr2 = Arr2 (\ e1 e2 -> f (getArr2 arr2 e1 e2))

instance Functor (Arr3 e1 e2 e3) where
  fmap f arr3 = Arr3 (\ e1 e2 e3 -> f (getArr3 arr3 e1 e2 e3))

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\ e1 e2 -> x)
-- :: f (a -> b) -> f a -> f b
  (<*>) f arr = Arr2 (\ e1 e2 -> (getArr2 f e1 e2) (getArr2 arr e1 e2))

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\ e1 e2 e3 -> x)
  (<*>) f arr = Arr3 (\ e1 e2 e3 -> (getArr3 f e1 e2 e3) (getArr3 arr e1 e2 e3))


--4
infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)