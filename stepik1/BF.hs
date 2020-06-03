module BrainFuck where

data Odd = Odd Integer deriving (Eq,Show)

instance Enum Odd where

-- :: Int -> Odd
  toEnum 0 = Odd 0
  toEnum x = Odd $ if x > 0 then (fromIntegral x) * 2 - 1 else (fromIntegral x) * 2 + 1
-- :: Odd -> Int
  fromEnum (Odd x) = if x > 0 then div (fromIntegral x + 1) 2 else div (fromIntegral x) 2
  succ (Odd x) = Odd (x+2)
  pred (Odd x) = Odd (x-2)
  enumFrom (Odd x) = map Odd [y | y <- [x ..], odd y]
  enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
  enumFromTo (Odd x) (Odd y) = map Odd [y | y <- [x .. y], odd y]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x, y .. z]


-- Тесты(с) run:  run
-- Большое число, которое не поместится в Int
bigOne = fromIntegral (maxBound :: Int) + 11 :: Integer

-- Генератор значений для тестирования
testVal n = Odd $ bigOne + n


testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10,
            test11, test12, test13, test14, test15, test16, test17]
allTests = zip [0..] testList
-- Список тестов с ошибками
run = do
  let result = map fst $ filter (not . snd) allTests
  let x = length result
  putStrLn $ "Hello! You are successful complete **" ++ show (17-x) ++ "** of 17 tests.\nFailed tests list: " ++ show result

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)

-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]


-- enumFromTo

-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]

-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []


-- enumFromThen

-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]


-- enumFromThenTo

-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]

-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]

-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []

-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3

-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)
