-- Ad hoc script for removing dupes in $PATH. Sample usage:
--
--   PATH=`remove-dupes-in-path $PATH`
--
-- To be generalized (e.g. delimiter) if needed.

import Data.List(intercalate, nub)
import Data.List.Split(splitOn)
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ case args of
    [x] -> intercalate ":" . nub . splitOn ":" $ x
    _   -> "You have to pass exactly one argument"

