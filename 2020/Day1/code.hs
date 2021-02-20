import Text.Printf (printf)

-- Input reading
readInput:: IO ([Int])
readInput = do
  r <- readFile "./Day1/input.txt"
  return [read l | l <- (lines r)]


-- Part 1
part1:: IO ()
part1 = do
  input <- readInput
  let (x, y) = sum2 input 2020
  putStrLn $ printf "(x, y) = (%d, %d)" x y
  putStrLn $ printf "x*y = %d" (x*y)

sum2:: [Int] -> Int -> (Int, Int)
sum2 [] _ = (0, 0)
sum2 (x:xs) r
  | any (== dif) xs   = (x, dif)
  | otherwise         = sum2 xs r
  where dif = r - x


-- Part 2
part2:: IO ()
part2 = do
  input <- readInput
  let (x, y, z) = sum3 input 2020
  putStrLn $ printf "(x, y, z) = (%d, %d, %d)" x y z
  putStrLn $ printf "x*y*z = %d" (x*y*z)

sum3:: [Int] -> Int -> (Int, Int, Int)
sum3 [] _ = (0, 0, 0)
sum3 (x:xs) r = case sum2 xs (r - x) of
  (0, 0) -> sum3 xs r
  (y, z) -> (x, y, z)
