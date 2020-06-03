module SimpleMath where

import Data.List

data Bit = O | I deriving Show
data Sign = N | P deriving Show
data Z = Z Sign [Bit]

instance Show Z where
  show (Z s b) = "Z" ++ show s ++ show b

n = Z P [I,O,I,I] -- 
n2 = Z P [I,I,I,I] -- 


pre (Z s b) = reverse . map f $ b where
  f I = 1
  f O = 0
toNum xs = fst ( foldr fx (0,0) xs ) where
  fx x (a, s) = (a + x*2^s, s+1)
z2num xs@(Z P b)= toNum . pre $ xs
z2num xs@(Z N b)= (0-) . toNum . pre $ xs

toReverseBin x = unfoldr f x
  where f x | x>0 && even x = Just (0, x `div` 2)
            | x>0 && odd x = Just (1, x `div` 2)
            | otherwise     = Nothing

num2z x = Z (if x > 0 then P else N) (map f (toReverseBin . abs $ x)) where
 f 1 = I :: Bit
 f 0 = O :: Bit


add :: Z -> Z -> Z
add a b = num2z(z2num(a) + z2num(b))

mul :: Z -> Z -> Z
mul a b = num2z(z2num(a) * z2num(b))


t = unfoldr (\x -> if x > 0 then Just (x, x-1) else Nothing)