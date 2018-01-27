import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad

-- Basic type
type MaleName = String
type FemaleName = String
type Name = String
type Score = Int

type Preference = (Name, [Name])
type MalePreference = (MaleName, [FemaleName])
type FemalePreference = (FemaleName, [MaleName])
type Pair = (MaleName, FemaleName)

-- Dictionary type
type Males = Map.Map MaleName Male
type Females = Map.Map FemaleName Status
type Scores = Map.Map FemaleName (Map.Map MaleName Score)

-- Data type
data Status = Single | Engaged String deriving (Show, Eq)
data Male = Male { choices :: [FemaleName]
                 , status :: Status } deriving (Show)

main :: IO()
main = do
    let breakPoint = "4"
    -- First item in tuple is the male/female name
    -- Second item is list of partner preference in order
    let pairs = [("0", ["7", "5", "6", "4"]),
                 ("1", ["5", "4", "6", "7"]),
                 ("2", ["4", "5", "6", "7"]),
                 ("3", ["4", "5", "6", "7"]), -- End of male
                 ("4", ["0", "1", "2", "3"]),
                 ("5", ["0", "1", "2", "3"]),
                 ("6", ["0", "1", "2", "3"]),
                 ("7", ["0", "1", "2", "3"])] -- End of female
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
    let (maleGroup, femaleGroup) = splitGroup pairs breakPoint
    consoleList "maleGroup:" maleGroup
    consoleList "femaleGroup:" femaleGroup

    let maleNames = takeNames maleGroup
    console "maleNames -> " maleNames

    let males = makeSingleMales maleGroup
    consoleList "males:" $ Map.toList males

    let females = makeSingleFemales femaleGroup
    consoleList "females:" $ Map.toList females

    let scores = makeScoreTable femaleGroup
    consoleList "scores:" $ Map.toList scores

    let (out, _) = matchAll maleNames scores females males True
    print out

matchAll :: [MaleName] -> Scores -> Females -> Males -> Bool -> (Females, Males)
matchAll maleNames scores females males True = let 
        (updatedFemaleStatus, updatedMaleProposals) = propose maleNames scores females males
        newCount = Map.size $ Map.filter (\a -> a /= Single) updatedFemaleStatus
        totalCount = Map.size updatedFemaleStatus 
        nextLoop = newCount /= totalCount
    in
        matchAll maleNames scores updatedFemaleStatus updatedMaleProposals nextLoop
matchAll _ _ females males False = (females, males)

propose :: [MaleName] -> Scores -> Females -> Males -> (Females, Males)
propose [] _ _ _ = (Map.empty, Map.empty)
propose [x] scores femaleStatuses males = case Map.lookup x males of
    -- Male is single, and still have several choices
    Just Male { choices = (y:ys), status = Single } -> (updatedFemaleStatuses, updatedMaleProposals) where
        -- Check if the female is available, if yes, then get the currently engaged male
        (isAvailable, currMale) = checkAvailability femaleStatuses y

        getScore = lookupScore scores
        oldMale = x

        isReplacable = getScore (oldMale, y) < getScore (currMale, y)

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
                                then acceptProposal femaleStatuses (y, oldMale)
                                else if isReplacable
                                    then acceptProposal femaleStatuses (y, oldMale)
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

checkAvailability :: Females -> FemaleName -> (Bool, String)
checkAvailability females name =
    case Map.lookup name females of
        Just (Single) -> (True, "")
        Just (Engaged m) -> (False, m)
        Nothing -> error "Not found"

acceptProposal :: Females -> Pair -> Females
acceptProposal femaleStatuses (femaleName, maleName) = 
    Map.insert femaleName (Engaged maleName) femaleStatuses

lookupScore :: Scores -> Pair -> Score
lookupScore scores (female, male) =
    if female == "" || male == "" 
        then 10
        else case Map.lookup female scores of
                Just maleScores ->
                    case Map.lookup male maleScores of
                        Just score -> score
                        Nothing -> 10
                Nothing -> 10

splitGroup :: [Preference] -> String -> ([MalePreference], [FemalePreference])
splitGroup groups breakPoint = 
    List.span (\(a, _) -> a /= breakPoint) groups

-- Take the first argument in the tuple, which is the name and return it
takeNames :: [Preference] -> [Name]
takeNames preferences = map fst preferences

-- Return a list of with Single assigned to each person
makeSingleFemales :: [Preference] -> Map.Map FemaleName Status
makeSingleFemales preferences = females where
    makeSingle = \a -> (fst a, Single)
    singles = map makeSingle preferences
    females = Map.fromList singles

-- Return a list of with Single assigned to each person
makeSingleMales :: [Preference] -> Map.Map MaleName Male
makeSingleMales preferences = males where
    makeSingle = \(male, choices) -> (male, Male { choices = choices
                                                 , status = Single })
    singles = map makeSingle preferences
    males = Map.fromList singles

makeScoreTable :: [FemalePreference] -> Scores
makeScoreTable preferences = scores where
    assignScore = \(name, preferences) -> (name, Map.fromList $ zip preferences [1..])
    femaleWithScores = map assignScore preferences
    scores = Map.fromList femaleWithScores

{-- UTILITIES
:contains useful utilities such as printing to IO(),
does not involve any business logic
--}

-- Prints a newline to IO()
br :: IO()
br = putStr "\n"

-- Prefix a with namespace before printing it to IO()
console :: (Show a) => String -> a -> IO()
console ns obj = do 
    putStr ns
    print obj
    br

-- Prefix list a with namespace before printing it to IO()
consoleList :: (Show a) => String -> [a] -> IO()
consoleList ns list = do
    putStrLn ns
    forM_ list $ \a -> print a
    br