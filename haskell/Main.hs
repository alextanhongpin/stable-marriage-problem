import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data Status = Single | Engaged String deriving (Show, Eq)
data Male = Male { choices :: [String]
                 , status :: Status } deriving (Show)

type MaleProposals = Map.Map String Male
type FemaleStatus = Map.Map String Status
type Scores = Map.Map String (Map.Map String Int)

main :: IO()
main = do
    let break = putStr "\n"
    let breakPoint = "4"
    let pairs = [("0", ["7", "5", "6", "4"]),
                ("1", ["5", "4", "6", "7"]),
                ("2", ["4", "5", "6", "7"]),
                ("3", ["4", "5", "6", "7"]),
                ("4", ["0", "1", "2", "3"]),
                ("5", ["0", "1", "2", "3"]),
                ("6", ["0", "1", "2", "3"]),
                ("7", ["0", "1", "2", "3"])]
    -- Expected results:
    -- '0', '7'
    -- '1', '5'
    -- '2', '4'
    -- '3', '6'

    -- let breakPoint = "a"
    -- let pairs = [("k", ["b", "c", "a"]),
    --             ("l", ["a", "c", "b"]),
    --             ("m", ["a", "b", "c"]),
    --             ("a", ["k", "l", "m"]),
    --             ("b", ["l", "m", "k"]),
    --             ("c", ["m", "l", "k"])]
    -- Expected results:
    -- 'k', 'c'
    -- 'a', 'l'
    -- 'b', 'm'

    let (maleGroup, femaleGroup) = List.span (\(a, _) -> a /= breakPoint) pairs
    let maleNames = map fst maleGroup
    putStr "Male Names -> "
    print maleNames
    break

    let femaleStatuses = Map.fromList $ map (\a -> (fst a, Single)) femaleGroup 
    putStr "Female statuses -> "
    print femaleStatuses
    break

    let males = Map.fromList $ map (\(male, choices) -> (male, Male { choices = choices
                                                                    , status = Single })) maleGroup
    -- putStr "Males -> "
    -- print males
    -- break

    let scores = Map.fromList $ map (\(name, preferences) -> (name, Map.fromList $ zip preferences [1..])) femaleGroup 

    let (out, _) = makeProposals maleNames scores femaleStatuses males True
    print out

makeProposals :: [String] -> Scores -> FemaleStatus -> MaleProposals -> Bool -> (FemaleStatus, MaleProposals)
makeProposals maleNames scores females males True = let 
        (updatedFemaleStatus, updatedMaleProposals) = propose maleNames scores females males
        newCount = Map.size $ Map.filter (\a -> a /= Single) updatedFemaleStatus
        totalCount = Map.size updatedFemaleStatus 
        nextLoop = newCount /= totalCount
    in
        makeProposals maleNames scores updatedFemaleStatus updatedMaleProposals nextLoop
makeProposals _ _ females males False = (females, males)

propose :: [String] -> Scores -> FemaleStatus -> MaleProposals -> (FemaleStatus, MaleProposals)
propose [] _ _ _ = (Map.empty, Map.empty)
propose [x] scores femaleStatuses males = case Map.lookup x males of
    -- Male is single, and still have several choices
    Just Male { choices = (y:ys), status = Single } -> (updatedFemaleStatuses, updatedMaleProposals) where
        -- Check if the female is available, if yes, then get the currently engaged male
        (isAvailable, currMale) = checkAvailability femaleStatuses y

        getScore = lookupScore scores y
        oldMale = x

        isReplacable = getScore oldMale < getScore currMale

        newEngagement = Map.insert oldMale Male { choices = ys, status = Engaged y } males

        breakup = Map.insert oldMale Male { choices = ys, status = Engaged y } males
        currMaleRecord = case Map.lookup currMale breakup of
            Just record -> record
            Nothing -> error "not found"
        updateMaleRecord x = x { status = Single }
        updateMaleEngagedRecord x = x { status = Engaged y }
        reengage = Map.insert currMale (updateMaleRecord currMaleRecord) breakup

        oldMaleRecord = case Map.lookup oldMale males of
            Just record -> record
            Nothing -> error "not found"
        upgradeProposal = Map.insert oldMale Male { choices = ys, status = Single } males
        upgradeProposal2 = Map.insert currMale (updateMaleEngagedRecord currMaleRecord) upgradeProposal


        updatedFemaleStatuses = if isAvailable
                                then acceptProposal femaleStatuses y oldMale
                                else if isReplacable
                                    then acceptProposal femaleStatuses y oldMale
                                    else femaleStatuses
        
        updatedMaleProposals = if isAvailable -- female is single
                                then newEngagement -- new match
                                else if isReplacable -- Old score is lower, means higher preference
                                    then reengage -- maintain the old relationship
                                    else upgradeProposal2 -- female choose a better one
    Just Male { status = Engaged _ } -> (femaleStatuses, males)
    Just Male { choices = [] } -> (femaleStatuses, males)
    Nothing -> error "cannot find"
propose (x:xs) scores femaleStatuses males = propose xs scores updatedFemaleStatuses output where
    (updatedFemaleStatuses, output) = propose [x] scores femaleStatuses males

checkAvailability :: FemaleStatus -> String -> (Bool, String)
checkAvailability femaleStatuses name =
    case Map.lookup name femaleStatuses of
        Just (Single) -> (True, "")
        Just (Engaged m) -> (False, m)

acceptProposal :: FemaleStatus -> String -> String -> FemaleStatus
acceptProposal femaleStatuses femaleName maleName = 
    Map.insert femaleName (Engaged maleName) femaleStatuses

lookupScore :: Scores -> String -> String -> Int
lookupScore scores female male =
    if female == "" || male == "" 
        then 10
        else case Map.lookup female scores of
                Just maleScores ->
                    case Map.lookup male maleScores of
                        Just score -> score
                        Nothing -> 10
                Nothing -> 10