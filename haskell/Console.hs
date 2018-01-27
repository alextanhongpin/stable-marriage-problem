{-- UTILITIES
:contains useful utilities such as printing to IO(),
does not involve any business logic
--}
module Console
( log
, list) where

import Control.Monad
import Prelude hiding (log)

-- Prints a newline to IO()
br :: IO()
br = putStr "\n"

-- Prefix a with namespace before printing it to IO()
log :: (Show a) => String -> a -> IO()
log ns obj = do 
    putStr ns
    print obj
    br

-- Prefix list a with namespace before printing it to IO()
list :: (Show a) => String -> [a] -> IO()
list ns xs = do
    putStrLn ns
    forM_ xs $ \a -> print a
    br
