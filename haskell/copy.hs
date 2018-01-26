import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


data Match = Engaged String String | Single String deriving (Show)
data Status = Available | NotAvailable String deriving (Show)

type Pair = (Match, [String])
type Pairs = [Pair]

type Approvals = Map.Map String Status
type Proposals = Map.Map String Status

type FemaleScore = Map.Map String (Map.Map String Int)
type Male = String
type Female = String

data Outcome = Success Approvals Proposals String Bool | Loyal | NewEngagement Approvals Proposals String Bool deriving (Show)

main :: IO()
main = do
    let break = putStr "\n"
    let pairs = [("0", ["7", "5", "6", "4"]),
                ("1", ["5", "4", "6", "7"]),
                ("2", ["4", "5", "6", "7"]),
                ("3", ["4", "5", "6", "7"]),
                ("4", ["0", "1", "2", "3"]),
                ("5", ["0", "1", "2", "3"]),
                ("6", ["0", "1", "2", "3"]),
                ("7", ["0", "1", "2", "3"])]

    let mappedPairs = map (\(a, b) -> (Single a, b)) pairs
    putStr "Mapped Pairs -> "
    print mappedPairs
    break

    let males = ["0", "1", "2", "3"]
    let females = ["4", "5", "6", "7"]

    let proposals = Map.fromList $ map (\name -> (name, Available)) males
    let approvals = Map.fromList $ map (\name -> (name, Available)) females
    putStr "Proposals -> "
    print proposals
    break

    -- Updating the status of the users to not available with the couple name
    print $ Map.insert "0" (NotAvailable "4") proposals

    putStr "Approvals -> "
    print approvals
    break

    -- Break the list to take the female group only
    putStr "Female scores -> "
    let (males, females) = List.span (\(a, b) -> a /= "4") pairs
    let femaleScores = Map.fromList $ map (\(a, b) -> (a, Map.fromList $ zip b [1..])) females
    print femaleScores
    break

    putStr "How female 5 rank man 2 -> "
    print $ score femaleScores "5" "2"
    break

    putStr "Guy 0 propose to female 4 -> "
    print $ propose proposals approvals femaleScores "0" "4"
    break

    -- putStr "Make Matches -> "
    -- let firstIteration = makeMatches mappedPairs
    -- let secondIteration = makeMatches firstIteration
    -- let thirdIteration = makeMatches secondIteration
    -- let fourthIteration = makeMatches thirdIteration
    -- print fourthIteration

propose :: Proposals -> Approvals -> FemaleScore -> String -> String -> Outcome
propose proposals approvals femaleScores m f = output where
    maleAvailability = Map.lookup m proposals
    femaleAvailibility = Map.lookup f approvals
    output = case maleAvailability of
        Just Available -> case femaleAvailibility of 
            -- Male and female is single and available
            Just Available -> Success newApprovals newProposals newStatus True where
                -- Female accept proposal
                newApprovals = Map.insert f (NotAvailable m) approvals

                -- Man successfully proposed
                newProposals = Map.insert m (NotAvailable f) proposals

                -- Update status
                newStatus = "male " ++ m ++ " and female " ++ f ++ " is available"
            
            -- Male is available, but female not. Female will have two proposals, and will rank it based on the score
            Just (NotAvailable m2) -> NewEngagement approvals proposals newStatus isChange where
                currScore = score femaleScores f m
                newScore = score femaleScores f m2
                
                statusChange = "male " ++ m ++ " is preferred over male " ++ m2
                -- TODO: Create a lookup table for female to score the guys and then return the one with the highest score
                -- If the guy has higher score than guy2, 
                -- update the status of the guy to be in a relationship with female
                -- update the status of guy2 to be single
                -- update the status of female to be in a relationship with guy
                statusMaintain = "male " ++ m ++ " is available, but female " ++ f ++ " is already engaged to male " ++ m2
                newStatus = if currScore < newScore 
                    then statusChange 
                    else statusMaintain
                isChange = currScore > newScore 
            -- Invalid
            Nothing -> error "cannot find the status of female specified"

        -- Man is already engaged, no futher obligations to propose. Skip
        Just (NotAvailable f2) -> Loyal
        -- Invalid
        Nothing -> error "cannot find the status of male specified"

-- Takes a list of single man names, and assign them to the Single type
makeSingle :: [String] -> [Match]
makeSingle matches =
    map (\a -> Single a) matches

makeMatch :: Pair -> Pair
-- If the male person is not engaged, and the female person is also not engaged
makeMatch (Single person, (x:xs)) = (Engaged person x, xs)
-- If the male person is engaged, skip the matching
makeMatch (Engaged p1 p2, (x:xs)) = (Engaged p1 x, xs)
-- If the male person is not engaged, but the female person is engaged


makeMatches :: Pairs -> Pairs
makeMatches [] = error "Empty list"
makeMatches [x] = [makeMatch x]
makeMatches (x:xs) = [makeMatch x] ++ makeMatches xs


score :: FemaleScore -> Female -> Male -> Int
score femaleScore female male = score where
    maleScores = Map.lookup female femaleScore
    score = case maleScores of 
                Just femaleScoreList -> out where
                    maleScore = Map.lookup male femaleScoreList
                    out = case maleScore of 
                            Just someScore -> someScore
                            Nothing -> 0
                Nothing -> 0