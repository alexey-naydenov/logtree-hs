module LogTree.Utils where

import LogTree.Data
import LogTree.Constants

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M

isOneOrTwoDigits :: T.Text -> Bool
isOneOrTwoDigits t =
  ((T.length t == 1) || (T.length t == 2)) && (T.all C.isDigit t) 

isTwoOrFourDigits :: T.Text -> Bool
isTwoOrFourDigits t =
  ((T.length t == 2) || (T.length t == 4)) && (T.all C.isDigit t) 

triLetterMonths :: S.Set T.Text
triLetterMonths = S.fromList $ pack ["jan", "feb", "mar", "apr", "may", "jun",
                                     "jul", "aug", "sep", "oct", "nov", "dec"]

isMonthName :: T.Text -> Bool
isMonthName t = S.member lowT triLetterMonths
  where lowT = T.toLower t

-- all permutations of 1,2,3
data Permutation1to3 =
  Perm123 | Perm213 |
  Perm132 | Perm312 |
  Perm231 | Perm321
  deriving (Eq, Ord, Show)

permutationsWithValueAtPosition :: Int -> Int -> S.Set Permutation1to3
permutationsWithValueAtPosition val pos
  | val == 1 && pos == 1 = S.fromList [Perm123, Perm132]
  | val == 1 && pos == 2 = S.fromList [Perm213, Perm312]
  | val == 1 && pos == 3 = S.fromList [Perm231, Perm321]
  | val == 2 && pos == 1 = S.fromList [Perm213, Perm231]
  | val == 2 && pos == 2 = S.fromList [Perm123, Perm321]
  | val == 2 && pos == 3 = S.fromList [Perm132, Perm312]
  | val == 3 && pos == 1 = S.fromList [Perm312, Perm321]
  | val == 3 && pos == 2 = S.fromList [Perm132, Perm231]
  | val == 3 && pos == 3 = S.fromList [Perm123, Perm213]
  | otherwise = S.empty

getDatePermutationsForPart :: (T.Text, Int) -> S.Set Permutation1to3
getDatePermutationsForPart (text, pos) =
  S.unions [permutationsWithValueAtPosition index pos
           | (index, predicate) <- indexedPredicates, predicate text]
  where
    indexedPredicates = [(1, isDay), (2, isMonth), (3, isYear)]

getDatePermutations :: [T.Text] -> S.Set Permutation1to3
getDatePermutations parts =
  foldr1 S.intersection $ fmap getDatePermutationsForPart $ zip parts [1, 2, 3]

isHourMinSec :: T.Text -> Bool
isHourMinSec t = (length parts == 3) && (all isOneOrTwoDigits parts)
  where parts = T.split (\ x -> x == ':') t

-- Public functions

pack :: [String] -> [T.Text]
pack = fmap T.pack

splitLine :: Char -> String -> [T.Text]
splitLine separator =
  filter (not . T.null) . T.split (\ x -> x == separator) . T.pack

splitLogLine :: T.Text -> [T.Text]
splitLogLine = filter (not . T.null) . T.split (\ x -> x == ' ' || x == '\t')

isDay :: T.Text -> Bool
isDay = isOneOrTwoDigits

isMonth :: T.Text -> Bool
isMonth t = (isOneOrTwoDigits t) || (isMonthName t)

isYear :: T.Text -> Bool
isYear = isTwoOrFourDigits

isDate :: T.Text -> Bool
isDate t = (length parts == 3) && (not . S.null . getDatePermutations $ parts)
  where parts = T.split (\ x -> x == '-') t

isTime :: T.Text -> Bool
isTime t = (isHourMinSec t)

removeCruft :: [(T.Text -> Bool)] -> [T.Text] -> [T.Text]
removeCruft preds parts = filter (\ p -> not $ any id (preds <*> pure p)) parts

dropSymbolsAround :: [Char] -> T.Text -> T.Text
dropSymbolsAround separators = T.dropAround (\x -> S.member x sepSet)
  where sepSet = S.fromList separators

-- convertLogEntryToPath :: String -> [T.Text]
-- convertLogEntryToPath entry = fmap (dropSymbolsAround ",[]{}\"'")
--   $ removeCruft [isTime, isDate] $ splitLine ' ' entry

convertLogEntryToPath :: T.Text -> [T.Text]
convertLogEntryToPath entry =
  fmap (dropSymbolsAround ",[]{}\"'") $
  removeCruft [isTime, isDate] $
  splitLogLine entry

buildChildMap :: [[T.Text]] -> (M.Map T.Text [[T.Text]])
buildChildMap entries = foldl insertChild M.empty entries
  where insertChild m (name:subchild)= M.insertWith (++) name [subchild] m
        insertChild m [] = m

buildLogTreeNode :: T.Text -> [[T.Text]] -> LogTree
buildLogTreeNode name [] = LogTree {logTreeValue = name, logTreeChildren = []}
buildLogTreeNode name paths =
  LogTree {logTreeValue = name, logTreeChildren = children}
  where children = [buildLogTreeNode n p | (n, p) <- M.assocs childrenMap]
        childrenMap = buildChildMap paths
        
