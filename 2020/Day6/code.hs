import Data.List.Split (splitOn)
import Data.List (nub)

type GroupForms = [String]


-- Input reading
readInput:: IO ([GroupForms])
readInput = do
  r <- readFile "./Day6/input.txt"
  return $ map lines (splitOn "\n\n" r)


-- Part 1
part1:: IO ()
part1 = do
  groupsForms <- readInput
  let res = sum [length (anyYes gFs) | gFs <- groupsForms]
  putStrLn $ show res

anyYes:: GroupForms -> String
anyYes = nub.concat


-- Part 2
part2:: IO ()
part2 = do
  groupsForms <- readInput
  let res = sum [length (allYes gFs) | gFs <- groupsForms]
  putStrLn $ show res

allYes:: GroupForms -> String
allYes fs = foldl f [] (head fs)
  where f ac c  | all (elem c) rest = ac ++ [c]
                | otherwise         = ac
        rest = tail fs
