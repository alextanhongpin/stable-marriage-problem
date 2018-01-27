import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- import qualified StableMarriageProblem as SMP
import qualified Console as Console

-- Basic type
type MaleName = String
type FemaleName = String
type Name = String
type BreakPoint = String
type Continue = Bool
type Score = Int

type Preference = (Name, [Name])
type MalePreference = (MaleName, [FemaleName])
type FemalePreference = (FemaleName, [MaleName])
type Pair = (MaleName, FemaleName)

-- Dictionary type
type Males = Map.Map MaleName Male
type Females = Map.Map FemaleName Female
type Scores = Map.Map MaleName Score

-- Data type
data Status = Single | Engaged String deriving (Show, Eq)

data Commitment = Available 
                | BreakUpToEngage MaleName MaleName 
                | MaintainEngagement deriving (Show, Eq)

data Male = Male { choices :: [FemaleName]
                 , status :: Status } deriving (Show)

data Female = Female { scores :: Scores
                     , statusFemale :: Status } deriving (Show)

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

-- stableMarriageProblem solves the stable marriage problem
stableMarriageProblem :: [Preference] -> BreakPoint -> (Females, Males)
stableMarriageProblem preferences breakPoint = matchAll females males True where
    (maleGroup, femaleGroup) = splitGroup preferences breakPoint
    males = makeSingleMales maleGroup
    females = makeSingleFemales femaleGroup

-- matchAll will iteratively match all Single males and females until a stable match has been 
-- reached
matchAll :: Females -> Males -> Continue -> (Females, Males)
matchAll females males True = matchAll females' males' continue where 
    (females', males') = propose (Map.keys males) females males
    continue = terminationCondition females'    
matchAll females males False = (females, males)

-- Iteratively make a proposal to guys that are still single. For every turn,
-- propose to a girl in the list of preference
propose :: [MaleName] -> Females -> Males -> (Females, Males)
propose [] _ _ = (Map.empty, Map.empty)
propose [x] females males = case Map.lookup x males of
    Just Male { choices = (y:ys), status = Single } -> case checkCommitment females (x, y) of
        Available -> (females', males') where
            females' = acceptProposal females (x, y)
            males' = makeProposals males (x, y, ys)

        BreakUpToEngage oldMale newMale -> (females', males') where
            females' = acceptProposal females (newMale, y)
            males' = Map.insert oldMale (makeSingle existingMale) engage where
                engage = Map.insert newMale Male { choices = ys, status = Engaged y } males
                existingMale = case Map.lookup oldMale engage of 
                    Just record -> record
                    Nothing -> error "not found"
                makeSingle x = x { choices = choices x, status = Single }

        MaintainEngagement -> (females, males') where
            males' = Map.insert x Male { choices = ys, status = Single } males

    Just Male { choices = [] } -> (females, males)
    Just Male { status = Engaged _ } -> (females, males)
    Nothing -> error "error occured at propose"
propose (x:xs) females males = propose xs females' males' where
    (females', males') = propose [x] females males

-- safeInit returns everything but the first item in the list
safeInit :: [Name] -> [Name]
safeInit [] = []
safeInit [y] = []
safeInit (y:ys) = ys

-- checkCommitment check if the female is engaged or single and return the Commitment type
checkCommitment :: Females -> Pair -> Commitment
checkCommitment females (maleName, femaleName) = case Map.lookup femaleName females of 
    Just Female { statusFemale = Single } -> Available
    Just Female { statusFemale = (Engaged existingMaleName)
                , scores = scores } -> decision where
        score1 = lookupScore scores maleName
        score2 = lookupScore scores existingMaleName
        breakup = score1 < score2
        decision = if breakup 
                    then BreakUpToEngage existingMaleName maleName 
                    else MaintainEngagement 
    Nothing -> error ("unable to check commitment for female " ++ femaleName)

-- acceptProposal updates the Female record to Engaged status
acceptProposal :: Females -> Pair -> Females
acceptProposal females (maleName, femaleName) = 
    case Map.lookup femaleName females of 
        Just Female { scores = scores } ->
            Map.insert femaleName Female { scores = scores
                                         , statusFemale = Engaged maleName } females
        Nothing -> females

-- makeProposal updates the Male record to Engaged status
makeProposals :: Males -> (MaleName, FemaleName, [FemaleName]) -> Males
makeProposals males (maleName, femaleName, femaleNames) =
    Map.insert maleName Male { choices = femaleNames
                             , status = Engaged femaleName } males

-- lookupScore returns the score of the male
lookupScore :: Scores -> MaleName -> Score
lookupScore scores maleName
    | maleName == "" = error "male cannot be empty"
    | otherwise = case Map.lookup maleName scores of
                    Just score -> score
                    Nothing -> error ("cannot get score for male " ++ maleName)

-- splitGroup partition a group into male and female
splitGroup :: [Preference] -> String -> ([MalePreference], [FemalePreference])
splitGroup groups breakPoint = 
    span (\(a, _) -> a /= breakPoint) groups

-- terminationCondition returns true if all male/female pairs has been matched
terminationCondition :: Females -> Continue
terminationCondition females = continue where
    single Female { statusFemale = status } = status == Single
    getSingleCount = Map.size . Map.filter single
    continue = getSingleCount females /= 0

-- makeSingleFemales returns a list of Female records that is assigned the single status
makeSingleFemales :: [Preference] -> Females
makeSingleFemales preferences = females where
    makeSingle (female, choices) = (female, Female { scores = scores
                                                   , statusFemale = Single }) where
        scores = Map.fromList $ zip choices [1..]
    femaleFrom = Map.fromList . map makeSingle 
    females = femaleFrom preferences

-- makeSingleMales returns a list of Male records that is assigned the single status
makeSingleMales :: [Preference] -> Males
makeSingleMales preferences = males where
    makeSingle (male, choices) = (male, Male { choices = choices
                                             , status = Single })
    maleFrom = Map.fromList . map makeSingle
    males = maleFrom preferences