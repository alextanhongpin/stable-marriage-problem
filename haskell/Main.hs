import qualified Data.Map as Map

import qualified StableMarriageProblem as SMP
import qualified Console as Console

main :: IO()
main = do
    let breakPoint1 = "4"
    let pairs1 = [("0", ["7", "5", "6", "4"]),
                 ("1", ["5", "4", "6", "7"]),
                 ("2", ["4", "5", "6", "7"]),
                 ("3", ["4", "5", "6", "7"]), -- End of male
                 ("4", ["0", "1", "2", "3"]),
                 ("5", ["0", "1", "2", "3"]),
                 ("6", ["0", "1", "2", "3"]),
                 ("7", ["0", "1", "2", "3"])] -- End of female

    let breakPoint2 = "a"
    let pairs2 = [("k", ["b", "c", "a"]),
                ("l", ["a", "c", "b"]),
                ("m", ["a", "b", "c"]),
                ("a", ["k", "l", "m"]),
                ("b", ["l", "m", "k"]),
                ("c", ["m", "l", "k"])]

    let stableMarriage1 = SMP.match pairs1 breakPoint1
    Console.list "stableMarriage1:" stableMarriage1
    -- Expected results:
    -- Married {male = "2", female = "4"}
    -- Married {male = "1", female = "5"}
    -- Married {male = "3", female = "6"}
    -- Married {male = "0", female = "7"}

    let stableMarriage2 = SMP.match pairs2 breakPoint2
    Console.list "stableMarriage2:" stableMarriage2
    -- Expected results:
    -- Married {male = "l", female = "a"}
    -- Married {male = "m", female = "b"}
    -- Married {male = "k", female = "c"}