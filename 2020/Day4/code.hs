import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Char (isDigit)

-- Input reading
readInput:: IO [[String]]
readInput = do
  r <- readFile "./Day4/input.txt"
  return $ map words (splitOn "\n\n" r)


-- Part 1
validPassport1:: [String] -> [String] -> Bool
validPassport1 kvs fields = all (\f -> elem f ks) fields
  where ks = map (takeWhile (/=':')) kvs

part1:: IO ()
part1 = do
  passports <- readInput
  let fields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
  let validPassports = sum [1 | p <- passports, validPassport1 p fields]
  putStrLn $ "Number of valid passports: " ++ (show validPassports)


-- Part 2
type Passport = Map.Map String String
type Rule = (String, (String -> Bool))

part2:: IO ()
part2 = do
  rawPassports <- readInput
  let passports = map readPassport rawPassports
  let rules = [ ("byr", byrPred)
              , ("iyr", iyrPred)
              , ("eyr", eyrPred)
              , ("hgt", hgtPred)
              , ("hcl", hclPred)
              , ("ecl", eclPred)
              , ("pid", pidPred)]

  let validPassports = sum [1 | p <- passports, validPassport2 p rules]
  putStrLn $ "Number of valid passports: " ++ show validPassports

readPassport:: [String] -> Passport
readPassport kvs = Map.fromList $ map readField kvs
  where readField kv =  let [k, v] = splitOn ":" kv
                        in (k, v)

validPassport2:: Passport -> [Rule] -> Bool
validPassport2 _ [] = True
validPassport2 pass ((k,p):rs) = case Map.lookup k pass of
  Just v  | p v       -> validPassport2 pass rs
          | otherwise -> False
  Nothing             -> False

byrPred, iyrPred, eyrPred, hgtPred, hclPred, eclPred, pidPred:: String -> Bool
byrPred s = checkNum s 4 1920 2002
iyrPred s = checkNum s 4 2010 2020
eyrPred s = checkNum s 4 2020 2030
hgtPred s = case unit of
  "cm" -> checkNum num 3 150 193
  "in" -> checkNum num 2 59 76
  _    -> False
  where (num, unit) = span isDigit s
hclPred s = h == '#' && all (\c -> isDigit c || elem c ['a'..'f']) t
  where h = head s
        t = tail s
eclPred s = elem s ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
pidPred s = (length s) == 9 && all isDigit s

checkNum:: String -> Int -> Int -> Int -> Bool
checkNum s len minN maxN
  | cond      = n >= minN && n <= maxN
  | otherwise = False
  where cond = all isDigit s && (length s) == len
        n = read s :: Int
