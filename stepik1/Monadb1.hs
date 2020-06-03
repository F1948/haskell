module Monad_b1 where
import qualified Data.Char as DC (isDigit)
import Prelude hiding (seq)
-- stapic 5.4

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show) 

--проверяет, является ли переданная строка 
--числом, "+", "-", "(", ")"
--Если да, то она возвращает нужное значение обёрнутое в Just,
--в противном случае - Nothing:
asToken :: String -> Maybe Token
asToken chars = 
  if all (`elem` ['0'..'9']) chars
  then Just $ Number $ read chars
  else case chars of
    "+" -> Just Plus
    "-" -> Just Minus
    "(" -> Just LeftBrace
    ")" -> Just RightBrace
    _   -> Nothing

seq = sequence
-- [m a] -> m [a]
tokenize :: String -> Maybe [Token]
tokenize input = seq $ map asToken (words input)
---
{-
Пусть имеется тип данных, который описывает конфигурацию шахматной доски:

data Board = ...

Кроме того, пусть задана функция

nextPositions :: Board -> [Board]

которая получает на вход некоторую конфигурацию доски и
возвращает все возможные конфигурации, которые могут получиться,
если какая-либо фигура сделает один ход.

Напишите функцию:

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]

которая принимает конфигурацию доски, число ходов n, предикат и
возвращает все возможные конфигурации досок, которые могут получиться,
если фигуры сделают n ходов и которые удовлетворяют заданному предикату.
При n < 0 функция возвращает пустой список.
-}

data Board = Board Int deriving (Show, Eq)

nextPositions :: Board -> [Board]
nextPositions xs = [xs, xs]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]


nextPositionsN b n pred | n == 0 = return b
nextPositionsN b n pred = do
  
  let board1 = nextPositions board
  board1 >>= (`f1` (n-1))
  filter pred res1 


pt = pythagoreanTriple
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    c <-[1..x]
    b <-[1..c]
    a <-[1..b]
    True <- return (a^2+b^2 == c^2)
--    True <- return (a < b && b < c)
    return (a,b,c)














