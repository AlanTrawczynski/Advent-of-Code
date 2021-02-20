
type Password = String
type Policy = (Int, Int, Char)

-- Input reading
readInput:: IO [(Policy, Password)]
readInput = do
  r <- readFile "./Day2/input.txt"
  return [readLine l | l <- lines r]

readLine:: String -> (Policy, Password)
readLine l = (readPolicy ns c, pw)
  where [ns, c, pw] = words l

readPolicy:: String -> String -> Policy
readPolicy ns c = (n', m', c')
  where (n, m) = break (=='-') ns
        n' = read n
        m' = read $ tail m
        c' = head c


-- Part 1
part1:: IO ()
part1 = do
  ls <- readInput
  let valids = sum [1 | l <- ls, validPassword1 l]
  putStrLn $ "Number of valid passwords = " ++ show valids

validPassword1:: (Policy, Password) -> Bool
validPassword1 ((n, m, c), pw) = occs >= n && occs <= m
  where occs = sum [1 | c2 <- pw, c == c2]


-- Part 2
part2:: IO ()
part2 = do
  ls <- readInput
  let valids = sum [1 | l <- ls, validPassword2 l]
  putStrLn $ "Number of valid passwords = " ++ show valids

validPassword2:: (Policy, Password) -> Bool
validPassword2 ((n, m, c), pw) = (checkChar n) + (checkChar m) == 1
  where checkChar i | pw !! (i-1) == c  = 1
                    | otherwise         = 0
