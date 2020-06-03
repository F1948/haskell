module FF where

import Data.List


reverseRange :: (Char,Char) -> String
reverseRange = unfoldr g 
  where
    g (a, b) = if a > b 
      then Nothing
      else Just (b, (a,pred b))
rR = reverseRange

charToInt :: Char -> Int
charToInt = read . (:[])


