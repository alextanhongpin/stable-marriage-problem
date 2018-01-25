import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


data Match = Engaged String String | Single String deriving (Show)

main :: IO()
main = do
    let list = [["7", "5", "6", "4"],
                ["5", "4", "6", "7"],
                ["4", "5", "6", "7"],
                ["4", "5", "6", "7"],
                ["0", "1", "2", "3"],
                ["0", "1", "2", "3"],
                ["0", "1", "2", "3"],
                ["0", "1", "2", "3"]]

    let listWithIntIndex = zip [0..] list
    let listWithStrIndex = map (\(a, b) -> (show a, b)) listWithIntIndex
    let (males, females) = List.span (\(a, b) -> a /= "4") listWithStrIndex
    let maleNames = map fst males 
    let femaleNames = map fst females
    print maleNames
    print femaleNames

    let maleScores = Map.fromList $ map (\(a, b) -> (a, Map.fromList $ zip b [1..])) females


    let malesWithPref = map (\(male, choices) -> (Single male, choices)) males
    print malesWithPref
    print $ map (\a -> match maleScores a) malesWithPref

    -- for each of the items, create a lookup table
    let lookupTable = Map.fromList listWithStrIndex
    print $ Map.lookup "0" lookupTable

    let np = Map.insert "0" False Map.empty
    print $ Map.insert "0" True np

type ScoreTable = Map.Map String (Map.Map String Int)
type Approve = Map.Map String Bool

match :: ScoreTable -> (Match, [String]) -> (Match, [String])
match _ (Single m, (x:xs)) = (Engaged m x, xs)
-- match table (Engaged m f, (x:xs)) = (Engaged output f, xs) where 
--     oldScore = score table f m
--     newScore = score table f x
--     output = if oldScore < newScore then m else x

score :: ScoreTable -> String -> String -> Int
score table m f =
    case Map.lookup f table of
        Just l -> case Map.lookup m l of 
            Just v -> v
            Nothing -> 0
        Nothing -> 0