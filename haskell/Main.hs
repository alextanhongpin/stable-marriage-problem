import qualified Data.Map as Map
import qualified Data.List as List

data Status = Single | Engaged String deriving (Show)
data Male = Male { choices :: [String]
                 , name :: String
                 , status :: Status } deriving (Show)

type MaleProposals = Map.Map String Male
type FemaleStatus = Map.Map String Status
type Scores = Map.Map String (Map.Map String Int)

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

    let (maleGroup, femaleGroup) = List.span (\(a, _) -> a /= "4") pairs
    let maleNames = map fst maleGroup
    putStr "Male Names -> "
    print maleNames
    break

    let femaleStatuses = Map.fromList $ map (\a -> (fst a, Single)) femaleGroup 
    putStr "Female statuses -> "
    print femaleStatuses
    break

    let males = Map.fromList $ map (\(male, choices) -> (male, Male { choices = choices
                                                                    , name = male
                                                                    , status = Single })) maleGroup
    -- putStr "Males -> "
    -- print males
    -- break

    let scores = Map.fromList $ map (\(name, preferences) -> (name, Map.fromList $ zip preferences [1..])) femaleGroup 
    -- putStr "Scores -> "
    -- print scores
    -- break

    -- putStr "Lookup score -> "
    -- print $ lookupScore scores "5" "0"
    -- break

    putStr "Propose -> "
    let (updatedFemaleStatuses, output) = propose maleNames scores femaleStatuses males
    -- print output
    print $ propose maleNames scores updatedFemaleStatuses output
    print "hello world"
    break

propose :: [String] -> Scores -> FemaleStatus -> MaleProposals -> (FemaleStatus, MaleProposals)
propose [] scores femaleStatuses males = (femaleStatuses, males)
propose [x] scores femaleStatuses males = case Map.lookup x males of
                                        -- Male is single - if female is also single, pair them up
                                        -- Else, female will rank the male and pick the one with the highest preference
                                        Just Male { choices = (y:ys)
                                                  , name = n
                                                  , status = Single } -> (updatedFemaleStatuses, updatedMap) where

                                                    -- Check if the female is available, if yes, then get the currently engaged male
                                                    (isAvailable, currMale) = checkAvailability femaleStatuses y

                                                    newEngagement = Map.insert x Male { choices = ys
                                                                    , name = n
                                                                    , status = Engaged y } males

                                                    oldMale = x

                                                    score1 = lookupScore scores y oldMale
                                                    score2 = lookupScore scores y newMale

                                                    newMale = if score1 < score2 then oldMale else currMale

                                                    -- Set one guy to be single, and another to be paired with the other one
                                                    breakup = Map.insert oldMale Male { choices = ys
                                                                                       , name = oldMale
                                                                                       , status = Single } males

                                                    breakup2 = if currMale /= "" then Map.insert currMale Male { choices = ys
                                                                                        , name = currMale
                                                                                        , status = Single } breakup else breakup

                                                    -- Engage the user to the new wive
                                                    engage2 = if newMale /= "" then Map.insert newMale Male { choices = ys
                                                                                      , name = newMale
                                                                                      , status = Engaged y } breakup2 else breakup2

                                                    updatedFemaleStatuses = if isAvailable
                                                                            then acceptProposal femaleStatuses y x 
                                                                            else femaleStatuses

                                                    updatedMap = if isAvailable
                                                                then newEngagement
                                                                else breakup2

                                        -- Male is engaged, skip
                                        Just Male { choices = (y:ys)
                                                  , name = n
                                                  , status = Engaged _ } -> (femaleStatuses, males)
                                        Nothing -> error "cannot find"

propose (x:xs) scores femaleStatuses males = propose xs scores updatedFemaleStatuses $ output where
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