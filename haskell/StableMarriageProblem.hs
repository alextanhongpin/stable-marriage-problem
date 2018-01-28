module StableMarriageProblem
(match) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

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

data Married = Married { male :: String
                       , female :: String } deriving (Show)

-- match solves the stable marriage problem
match :: [Preference] -> BreakPoint -> [Married]
match preferences breakPoint = stableMarriage where
    (maleGroup, femaleGroup) = splitGroup preferences breakPoint
    males = makeSingleMales maleGroup
    females = makeSingleFemales femaleGroup
    (females', males') = matchAll females males True
    stableMarriage = map (\(female, Female { statusFemale = Engaged male }) -> Married { male = male, female = female }) $ Map.toList females'

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
            males' = lookupAndUpdateMaleStatus engage oldMale Single where
                engage = updateMale males newMale ys (Engaged y)

        MaintainEngagement -> (females, males') where
            males' = updateMale males x ys Single

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


updateMale :: Males -> MaleName -> [FemaleName] -> Status -> Males
updateMale males maleName choices status =
    Map.insert maleName Male { choices = choices, status = status } males

lookupAndUpdateMaleStatus :: Males -> MaleName -> Status -> Males
lookupAndUpdateMaleStatus males maleName status = Map.insert maleName (updateStatus found) males where
    found = case Map.lookup maleName males of
                Just record -> record
                Nothing -> error "not found"
    updateStatus x = x { choices = choices x, status = status }
    

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