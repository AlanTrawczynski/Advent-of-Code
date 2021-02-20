import Text.Printf (printf)

-- Input reading
readInput:: IO ([Int])
readInput = do
  r <- readFile "./Day9/input.txt"
  return $ map read (lines r)


-- Part 1
part1:: IO ()
part1 = do
  ns <- readInput
  let w = weakNum ns
  putStrLn $ "The weakness number is: " ++ show w

weakNum:: [Int] -> Int
weakNum xs
  | isWeak prev n = n
  | otherwise     = weakNum $ tail xs
  where prev = take 25 xs
        n = xs !! 25

isWeak:: [Int] -> Int -> Bool
isWeak [] _ = True
isWeak (x:xs) s
  | any (== dif) xs   = False
  | otherwise         = isWeak xs s
  where dif = s - x


-- Part 2
part2:: IO ()
part2 = do
  ns <- readInput
  let w = weakNum ns
      wSet = weakSet ns w
      minN = minimum wSet
      maxN = maximum wSet

  putStrLn $ "The contiguous set is: " ++ show wSet
  putStrLn $ printf "Smallest + largest = %d + %d = %d" minN maxN (minN + maxN)

weakSet:: [Int] -> Int -> [Int]
weakSet (n:ns) s = case takeUntilSum ns s n [n] of
  Just set  -> set
  Nothing   -> weakSet ns s

takeUntilSum:: [Int] -> Int -> Int -> [Int] -> Maybe [Int]
takeUntilSum (n:ns) s ac set
  | ac < s    = takeUntilSum ns s (ac+n) (n:set)
  | ac == s   = Just set
  | otherwise = Nothing
