import qualified Data.List as List
import qualified Data.Map as Map

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

    -- for each of the items, create a lookup table
    let lookupTable = Map.fromList listWithStrIndex
    print $ Map.lookup "0" lookupTable
    
    -- for each of the male candidates, pair them up with the first choice




--     let matchesMap = Map.fromList listWithStrIndex
--     print $ Map.lookup "0" matchesMap
--     let k = Map.insert 0 True Map.empty
--     print k

--     print $ match ["1", "2", "3"] Map.empty
  
-- match :: [String] -> Map.Map String Bool -> Map.Map String Bool
-- match [] map = map
-- match (x:xs) map = match xs $ Map.insert x True map
  