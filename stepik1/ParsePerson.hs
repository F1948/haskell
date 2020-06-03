module ParsePerson where
-- stepik 4.4

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
  deriving Show
data Person = Person { firstName :: String, lastName :: String, age :: Int }
  deriving Show


-- f ("", "abc/ndef") -> ("abc", "def"); split by '\n'
splitCR :: (String, String) -> (String, String)
splitCR (x, []) = (reverse x, [])
splitCR (x,(a:xs)) =
  if [a] /= "\n"
  then splitCR (a:x, xs)
  else (reverse x, xs)


-- f ("", "abc = def") -> ("abc", "def"); split by ' = '
splitEQ :: (String, String) -> (String, String)
splitEQ (x, []) = (reverse x, [])
splitEQ (x, (a:[])) = (reverse (a:x), "")
splitEQ (x, (a:b:[])) = (reverse (b:a:x), "")
splitEQ (x,(a:b:c:xs)) =
  if a:b:c:[] /= " = "
  then splitEQ (a:x, b:c:xs)
  else (reverse x, xs)



firstSplitter :: String -> [String]
firstSplitter [] = []
firstSplitter xs = a: firstSplitter b
  where (a, b) = splitCR ("", xs)

secondSplitter :: [String] -> Maybe [(String, String)]
secondSplitter x | flag = Nothing | otherwise = Just f
  where
    f = map (curry splitEQ "") x -- [("",""), ("","")]
    flag = any (\(a,b) -> null a || null b) f

-- f = secondSplitter . firstSplitter
-- f "a = 1\nb = 2\n c =8" -> Nothing
-- f "a = 1\nb = 2\n c = 8" -> Just [("a","1"),("b","2"),(" c","8")]

--ageIsDigit :: String -> Maybe String
ageIsDigit = all (`elem` ['0'..'9'])


main :: String -> Either Error Person
main x = case secondSplitter . firstSplitter $ x of
  Nothing -> Left ParsingError
  Just array -> case lookup "firstName" array of
    Nothing -> Left IncompleteDataError
    Just firstNameFild -> case lookup "lastName" array of
      Nothing -> Left IncompleteDataError
      Just lastNameFild -> case lookup "age" array of
        Nothing -> Left IncompleteDataError
        Just ageFild -> case ageIsDigit ageFild of
          False -> Left (IncorrectDataError ageFild)
          True -> 
            Right Person { firstName = firstNameFild, lastName = lastNameFild, age = read ageFild}


parsePerson :: String -> Either Error Person
parsePerson = main



{-

Задание громоздкое, но не сложное. Проверяем ровно то, что спрашивают. Всё лишнее отбрасываем:
Разбили строку по "\n" получили ровно три строки - хорошо, иначе кидаем ошибку.
Проверили, что все строки не пустые.
Каждую по " = " разбили на название поля и содержимое.
Проверили, что названия совпадают с эталонами, а содержимое не пусто (порядок фиксирован).
Проверили, что возраст - это число.
Всё, что осталось сложили в Person.

-}