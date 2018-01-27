import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad
-- import qualified StableMarriageProblem as SMP

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
-- Whether the female party wants to maintain the relationship or breakup
-- with the current partner
data Commitment = Available | BreakUpToEngage MaleName MaleName | MaintainEngagement deriving (Show, Eq)

main :: IO()
main = do

    -- let m = Male { choices = ["1", "2"], status = Single }
    -- let makeEngage x y = x { status = Engaged y } 
    -- let makeSingle x = x { status = Single }
    -- let reduceChoice x = x { choices = init $ choices x }
    -- print m
    -- let m2 = makeEngage m "2"
    -- print m2
    -- let m3 = makeSingle m2
    -- print m3
    -- let m4 = reduceChoice m3
    -- print m4



    -- print $ SMP.sumItNow 1
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

-- matchAll will iteratively match all Single males and females until a stable match has been 
-- reached
matchAll :: [MaleName] -> Scores -> Females -> Males -> Bool -> (Females, Males)
matchAll maleNames scores females males True = let 
        (updatedFemales, updatedMales) = propose maleNames scores females males
        continue = terminationCondition updatedFemales
    in
        matchAll maleNames scores updatedFemales updatedMales continue
matchAll _ _ females males False = (females, males)

-- Iteratively make a proposal to guys that are still single. For every turn,
-- propose to a girl in the list of preference
propose :: [MaleName] -> Scores -> Females -> Males -> (Females, Males)
propose [] _ _ _ = (Map.empty, Map.empty)
propose [x] scores femaleStatuses males = case Map.lookup x males of
    -- Male is single, and still have several choices
    Just Male { choices = (y:ys), status = Single } -> (updatedFemaleStatuses, updatedMaleProposals) where

        -- utilities helper to update the state of the records
        -- reduceChoices x = x { choices = ys }
        -- makeSingle x = x { status = Single }
        -- makeEngaged x = x { status = Engaged y }

        -- checkCommitment females scores (x, y) 
        --     | Available = 
        --     | BreakUpToEngage m1 m2 = 
        --     | MaintainEngagement =

        femaleName = y
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
                                then acceptProposal femaleStatuses (oldMale, y)
                                else if isReplacable
                                    then acceptProposal femaleStatuses (oldMale, y)
                                    else femaleStatuses
        
        updatedMaleProposals = if isAvailable -- female is single
                                then newEngagement -- new match
                                else if isReplacable -- Old score is lower, means higher preference
                                    then reengage -- maintain the old relationship
                                    else upgradeProposal2 -- female choose a better one
    Just Male { status = Engaged _ } -> (femaleStatuses, males)
    Just Male { choices = [] } -> (femaleStatuses, males)
    Nothing -> error "error occured at propose"
propose (x:xs) scores females males = propose xs scores updatedFemales updatedMales where
    (updatedFemales, updatedMales) = propose [x] scores females males

-- Check if the female is available or already engaged to a male
checkAvailability :: Females -> FemaleName -> (Bool, String)
checkAvailability females name =
    case Map.lookup name females of
        Just (Single) -> (True, "")
        Just (Engaged m) -> (False, m)
        Nothing -> error "Not found"

-- Check the female commitment
-- checkCommitment :: Females -> Scores -> Pair -> Commitment
-- checkCommitment females (maleName, femaleName) = state where
--     case Map.lookup femaleName females of 
--         Just Single -> Available
--         Just (Engaged existingMaleName) -> decision where
--             -- compute the score of the female against the currMale
--             score1 = lookupScore scores (maleName, femaleName) 
--             score2 = lookupScore scores (existingMaleName, femaleName)
--             breakup = score1 < score2
--             decision = if breakup 
--                 then BreakUpToEngage existingMaleName maleName 
--                 else MaintainEngagement 
--         Nothing -> error ("unable to check commitment for female " ++ femaleName)

acceptProposal :: Females -> Pair -> Females
acceptProposal females (maleName, femaleName) = 
    Map.insert femaleName (Engaged maleName) females

-- Lookup the score for a male-female pair and return them
-- TODO: It is not possible to get max inf score in haskell, 
-- one solution is to probably assign the ORD such as Eq, Gt, or LT
-- for comparison
lookupScore :: Scores -> Pair -> Score
lookupScore scores (male, female)
    | female == "" = error "female cannot be empty"
    | male == "" = error "male cannot be empty"
    | otherwise = case Map.lookup female scores of
                    Just maleScores ->
                        case Map.lookup male maleScores of
                            Just score -> score
                            Nothing -> error ("cannot get score for male " ++ male)
                    Nothing -> error ("cannot get score for female " ++ female)

-- Take a group of male and female preferences and return them in two partition
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
    makeSingle = \(male, choices) -> (male, 
                                      Male { choices = choices
                                           , status = Single })
    singles = map makeSingle preferences
    males = Map.fromList singles

-- Assign scores to each male and return a dictionary lookup
makeScoreTable :: [FemalePreference] -> Scores
makeScoreTable preferences = scores where
    assignScore = \(name, preferences) -> (name, Map.fromList $ zip preferences [1..])
    computeScores =  Map.fromList . map assignScore
    scores = computeScores preferences

-- Checks if the termination condition is reached, which is when all male-female is engaged
terminationCondition :: Females -> Bool
terminationCondition females = isTerminated where
    getTotalCount = Map.size . Map.filter (\a -> a /= Single)
    currCount = getTotalCount females
    totalCount = Map.size females
    isTerminated = currCount /= totalCount

-- testOrder :: Int -> Int -> Ordering
-- testOrder a b 
--     | a > b = GT
--     | a < b = LT
--     | a == b = EQ

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
