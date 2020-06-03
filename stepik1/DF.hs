module DataFramework where

import qualified Data.List as DL

-- модуль для экспериментов с data, type, instance, newtype и т.д.
-- data Person a - Person есть конструктор типа (Person Int - уже тип)
data Race = Human | Orc | Elf
instance Show Race where
  show Human = "Human"
  show Orc = "Orc"
  show Elf = "Elf"

data Person = Person { race :: Race, name :: String, age :: Int } -- Person - конструктор значений
instance Show Person where
  show (Person r n a) = "\nrace: "++show r++"\nname: "++n++"\nage: "++show a++"\n"

type Band = [Person] -- определение синонима типа (type String = [Char])

ed = Person Orc "Ed" 17
bill = Person Elf "Bill" 124
john = Person Human "John" 33

f :: Int -> [Int] -> Int
f x xs = (maybe 0 id ) $ flip DL.find xs (==x)

g :: [Bool] -> Int -> [Int]
g [] _ = []
g (x:xs) n = if x then n : g xs (n+1) else g xs (n+1)

g2 :: (Eq a) => [a] -> [a] -> [Bool]
g2 x y = map (`elem` y) x


--data TD a = TD a where

toX a = zipWith (\x y -> x + y) a
x = toX [1,100,1000]
expand xs = x (cycle [xs])
