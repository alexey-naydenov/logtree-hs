module LogTree.Utils (
  splitLine,
  pack,
  isDay,
  isMonth,
  isYear,
  isDate
  ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set as S

isOneOrTwoDigits :: T.Text -> Bool
isOneOrTwoDigits t =
  ((T.length t == 1) || (T.length t == 2)) && (T.all C.isDigit t) 

isTwoOrFourDigits :: T.Text -> Bool
isTwoOrFourDigits t =
  ((T.length t == 1) || (T.length t > 0)) && (T.all C.isDigit t) 

triLetterMonths :: S.Set T.Text
triLetterMonths = S.fromList $ pack ["jan", "feb", "mar", "apr", "may", "jun",
                                     "jul", "aug", "sep", "oct", "nov", "dec"]

isMonthName :: T.Text -> Bool
isMonthName t = S.member lowT triLetterMonths
  where lowT = T.toLower t

-- isSquare :: [[a]] -> Bool
-- isSquare [[]] = False
-- isSquare [[_]] = True
-- isSquare x = all (\ l -> l == length lengths) lengths
--   where lengths = fmap length x

hasTrueRows :: [[Bool]] -> Bool
hasTrueRows x = all id (fmap (any id) x)

hasTrueColumns :: [[Bool]] -> Bool
hasTrueColumns = hasTrueRows . L.transpose

-- Public functions

pack :: [String] -> [T.Text]
pack = fmap T.pack

splitLine :: Char -> String -> [T.Text]
splitLine separator =
  filter (not . T.null) . T.split (\ x -> x == separator) . T.pack

isDay :: T.Text -> Bool
isDay = isOneOrTwoDigits

isMonth :: T.Text -> Bool
isMonth t = (isOneOrTwoDigits t) || (isMonthName t)

isYear :: T.Text -> Bool
isYear = isTwoOrFourDigits

isDate :: T.Text -> Bool
isDate t = (length parts == 3) && hasTrueRows kinds && hasTrueColumns kinds
  where parts = T.split (\ x -> x == '-') t
        kinds = [fmap x parts | x <- [isDay, isMonth, isYear]]
  
