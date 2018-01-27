import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- import qualified StableMarriageProblem as SMP
import qualified Console as Console

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
data Status = Single 
            | Engaged String deriving (Show, Eq)

-- Whether the female party wants to maintain the relationship or breakup
-- with the current partner
data Commitment = Available 
                | BreakUpToEngage MaleName MaleName 
                | MaintainEngagement deriving (Show, Eq)

data Male = Male { choices :: [FemaleName]
                 , status :: Status } deriving (Show)


main :: IO()
main = do
    -- print $ SMP.sumItNow 1
    -- First item in tuple is the male/female name
    -- Second item is list of partner preference in order
    let breakPoint1 = "4"
    let pairs1 = [("0", ["7", "5", "6", "4"]),
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

    let breakPoint2 = "a"
    let pairs2 = [("k", ["b", "c", "a"]),
                ("l", ["a", "c", "b"]),
                ("m", ["a", "b", "c"]),
                ("a", ["k", "l", "m"]),
                ("b", ["l", "m", "k"]),
                ("c", ["m", "l", "k"])]
    -- Expected results:
    -- 'k', 'c'
    -- 'a', 'l'
    -- 'b', 'm'
    let (females, males) = stableMarriageProblem pairs1 breakPoint1
    Console.list "females:" $ Map.toList females
    Console.list "males:" $ Map.toList males

    let (females2, males2) = stableMarriageProblem pairs2 breakPoint2
    Console.list "females2:" $ Map.toList females2
    Console.list "males2:" $ Map.toList males2

stableMarriageProblem :: [Preference] -> String -> (Females, Males)
stableMarriageProblem preferences breakPoint = let
        (maleGroup, femaleGroup) = splitGroup preferences breakPoint
        maleNames = takeNames maleGroup
        males = makeSingleMales maleGroup
        females = makeSingleFemales femaleGroup
        scores = makeScoreTable femaleGroup
    in
        matchAll maleNames scores females males True

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
propose [x] scores females males = case Map.lookup x males of
    -- Male is single, and still have several choices
    Just Male { choices = (y:ys), status = Single } -> case checkCommitment females scores (x, y) of
        Available -> (updatedFemales, updatedMales) where
            updatedFemales = acceptProposal females (x, y)
            updatedMales = makeProposals males (x, y, ys)

        BreakUpToEngage oldMale newMale -> (updatedFemales, updatedMales) where
            updatedFemales = acceptProposal females (newMale, y)
            updatedMales = Map.insert oldMale (makeSingle existingMale) engage where
                engage = Map.insert newMale Male { choices = ys, status = Engaged y } males
                existingMale = case Map.lookup oldMale engage of 
                    Just record -> record
                    Nothing -> error "not found"
                makeSingle x = x { choices = choices x, status = Single }

        MaintainEngagement -> (females, updatedMales) where
            updatedMales = Map.insert x Male { choices = ys, status = Single } males
    Just Male { status = Engaged _ } -> (females, males)
    Just Male { choices = [] } -> (females, males)
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

safeInit :: [Name] -> [Name]
safeInit [] = []
safeInit [y] = []
safeInit (y:ys) = ys

-- Check the female commitment
checkCommitment :: Females -> Scores -> Pair -> Commitment
checkCommitment females scores (maleName, femaleName) = case Map.lookup femaleName females of 
    Just Single -> Available
    Just (Engaged existingMaleName) -> decision where
        -- compute the score of the female against the currMale
        score1 = lookupScore scores (maleName, femaleName) 
        score2 = lookupScore scores (existingMaleName, femaleName)
        breakup = score1 < score2
        decision = if breakup 
            then BreakUpToEngage existingMaleName maleName 
            else MaintainEngagement 
    Nothing -> error ("unable to check commitment for female " ++ femaleName)

acceptProposal :: Females -> Pair -> Females
acceptProposal females (maleName, femaleName) = Map.insert femaleName (Engaged maleName) females

makeProposals :: Males -> (MaleName, FemaleName, [FemaleName]) -> Males
makeProposals males (maleName, femaleName, femaleNames) =
    Map.insert maleName Male { choices = femaleNames
                             , status = Engaged femaleName } males

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

