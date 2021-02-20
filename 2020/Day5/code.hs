import Data.List (sort)

type Seat = (Int, Int)

seatId:: Seat -> Int
seatId (r,c) = 8*r + c

readSeat:: String -> Seat
readSeat s = (row, col)
  where binS = [b | c <- s, let b = if c == 'B' || c == 'R' then 1 else 0]
        row = binToInt $ take 7 binS
        col = binToInt $ drop 7 binS

binToInt:: [Int] -> Int
binToInt ns = sum [x*(2^i) | (x, i) <- zip ns ls]
  where len = length ns
        ls = [len-1, len-2..0]


-- Input reading
readInput:: IO [Seat]
readInput = do
  r <- readFile "./Day5/input.txt"
  return $ [readSeat x | x <- lines r]


-- Part 1
part1:: IO ()
part1 = do
  seats <- readInput
  let maxId = maximum $ map seatId seats
  putStrLn $ "Highest seat ID: " ++ show maxId


-- Part 2
part2:: IO ()
part2 = do
  seats <- readInput
  putStrLn $ "My seat ID: " ++ (show $ mySeatId seats)

mySeatId:: [Seat] -> Int
mySeatId xs = mySeatId' xs'
  where xs' = sort $ map seatId xs

mySeatId':: [Int] -> Int
mySeatId' [] = error "The plane is full"
mySeatId' (x:y:xs)
  | y == x+2  = x+1
  | otherwise = mySeatId' (y:xs)
