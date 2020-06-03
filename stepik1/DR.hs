module DataRecords where

import Data.Char(isDigit)

import Data.Time.Clock
import Data.Time.Format
--import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {
                          timestamp :: UTCTime
                         , logLevel :: LogLevel
                         , message :: String
                         }

instance Show LogEntry where
  show (LogEntry ts ll m) = timeToString ts ++ ": " ++ show ll ++ ": " ++ m

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString = show

t =
  let ct = read "2019-02-24 18:28:52.607875 UTC"::UTCTime
      le = LogEntry ct Info "Info Message"
  in logEntryToString le



data Person = Person { firstName :: String, lastName :: String, age :: Int }
instance Show Person where
  show (Person a b c) = b ++ " " ++ a ++ ", age " ++ show c


updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

-- (a : _ : _) - паттерн матч на НЕ одноэлементный список.
abbrFirstName :: Person -> Person
abbrFirstName thePerson@(Person { firstName = firstName }) =
  thePerson {firstName = updater} where
    updater | length firstName < 2 = [head firstName]
            | otherwise = head firstName : "."

data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter cellSize (Coord x y) = Coord (f x) (f y) where
    f x = cellSize * (fromIntegral x) + cellSize * 1/2

getCell :: Double -> Coord Double -> Coord Int
getCell cellSize (Coord x y) = Coord (f x) (f y) where
  f x = floor (x / cellSize) :: Int




findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | isDigit x = Just x
                 | otherwise = findDigit xs







