
-- Input reading
readInput:: IO [String]
readInput = do
  r <- readFile "./Day3/input.txt"
  return $ lines r


-- Part 1
part1:: IO ()
part1 = do
  rows <- readInput
  let trees = countTrees1 rows
  putStrLn $ "Number of trees = " ++ show trees

countTrees1:: [String] -> Int
countTrees1 rows = fst $ foldl f (0,3) (tail rows)
  where l = length $ head rows
        f (n,i) r | r !! i == '#' = (n+1, i')
                  | otherwise     = (n,   i')
                  where i' = (i+3) `mod` l


-- Part 2
type Slope = (Int, Int)

part2:: IO ()
part2 = do
  rows <- readInput
  let slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]
  let trees = [countTrees2 rows s | s <- slopes]

  putStrLn $ "(slope, trees) = " ++ (unwords $ map show (zip slopes trees))
  putStrLn $ "Product = " ++ (show $ product trees)

countTrees2:: [String] -> Slope -> Int
countTrees2 rows s@(x,y) = countTrees2' rows' s x
  where rows' = drop y rows

countTrees2':: [String] -> Slope -> Int -> Int
countTrees2' [] _ _ = 0
countTrees2' (r:rs) s@(x, y) i
  | infR !! i == '#'  = countTrees2' rs' s i' + 1
  | otherwise         = countTrees2' rs' s i'
    where infR = concat $ repeat r
          rs' = drop (y-1) rs
          i' = i + x
