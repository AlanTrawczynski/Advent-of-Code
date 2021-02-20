import Data.Char (isLetter, isDigit)
import Data.List.Split (splitOn)
import qualified Data.Map as Map


type Bags = Map.Map String [(String, Int)]

-- Input reading
readInput:: IO Bags
readInput = do
  r <- readFile "./Day7/input.txt"
  return $ Map.fromList [readLine l | l <- lines r]

readLine:: String -> (String, [(String, Int)])
readLine line = (bag, readContent containedBags)
  where [bag, containedBags] = splitOn " bags contain " line

readContent:: String -> [(String, Int)]
readContent s = case s of
  "no other bags."  -> []
  _                 -> map (readContent'.init.words) (splitOn "," s)

readContent':: [String] -> (String, Int)
readContent' (n:ws) = (bag, read n)
  where bag = unwords ws


-- Part 1
part1:: IO ()
part1 = do
  bags <- readInput
  let query = "shiny gold"
      n = length $ containedBy query bags

  putStrLn $ "Number of bag colors that can contain a " ++ query ++ " bag: " ++ show n

containedBy:: String -> Bags -> [String]
containedBy q bags = Map.foldrWithKey f [] bags
  where f k v ac  | containsBag q v bags  = k:ac
                  | otherwise             = ac

containsBag:: String -> [(String, Int)] -> Bags -> Bool
containsBag q bs bags = contains || subcontains
  where contains = elem q contBags
        subcontains = any (\b -> containsBag q b bags) (map (bags Map.!) contBags)
        contBags = map fst bs


-- Part 2
part2:: IO ()
part2 = do
    bags <- readInput
    let query = "shiny gold"
        required = requiredInside query bags
        n = sum $ map snd (Map.toList $ required)

    print required
    putStrLn $ "Number of bags required inside a " ++ query ++ " bag: " ++ show n

requiredInside:: String -> Bags -> Map.Map String Int
requiredInside q bags = requiredInside' req bags Map.empty
  where req = bags Map.! q

requiredInside':: [(String, Int)] -> Bags -> (Map.Map String Int) -> (Map.Map String Int)
requiredInside' [] _ ac = ac
requiredInside' ((b, n):qs) bags ac = requiredInside' qs' bags ac'
  where ac' = Map.insertWith (+) b n ac
        qs' = qs ++ requiredBags
        requiredBags = map (\(b2, n2) -> (b2, n*n2)) (bags Map.! b)
