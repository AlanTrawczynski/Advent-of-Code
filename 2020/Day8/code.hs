import Data.Array


data Instruction = Acc Int | Jmp Int | Nop Int
  deriving Show

modAcc:: Instruction -> Int -> Int
modAcc i acc = case i of
  Acc n   -> acc + n
  _       -> acc

nextPos:: Instruction -> Int -> Int
nextPos i p = case i of
  Jmp n   -> p + n
  _       -> p + 1


-- Input reading
readInput:: IO (Array Int (Instruction, Bool))
readInput = do
  r <- readFile "./Day8/input.txt"
  let is = map (\l -> (readInstruction l, False))  (lines r)
      len = length is
  return $ listArray (0, len-1) is

readInstruction:: String -> Instruction
readInstruction line = case inst of
  "acc" -> Acc n'
  "jmp" -> Jmp n'
  "nop" -> Nop n'
  where [inst, n] = words line
        n' = read (filter (/='+') n) :: Int


-- Part 1
part1:: IO ()
part1 = do
  is <- readInput
  let acc = accBeforeLoop is
  putStrLn $ "Accumulator value is: " ++ show acc

accBeforeLoop:: Array Int (Instruction, Bool) -> Int
accBeforeLoop is = accBeforeLoop' is 0 0

accBeforeLoop':: Array Int (Instruction, Bool) -> Int -> Int -> Int
accBeforeLoop' is p acc
  | exec      = acc
  | otherwise = accBeforeLoop' is' (nextPos i p) (modAcc i acc)
  where (i, exec) = is ! p
        is' = is // [(p, (i,True))]


-- Part 2
part2:: IO ()
part2 = do
  is <- readInput
  let acc = accWithoutLoop is
  putStrLn $ "Accumulator value after program termination is: " ++ show acc

accWithoutLoop:: Array Int (Instruction, Bool) -> Int
accWithoutLoop is = accWithoutLoop' is 0

accWithoutLoop':: Array Int (Instruction, Bool) -> Int -> Int
accWithoutLoop' is p = case executeProgram is' of
  Nothing   -> accWithoutLoop' is p'
  Just acc  -> acc
  where (is', p') = fixNextInst is p

fixNextInst:: Array Int (Instruction, Bool) -> Int -> (Array Int (Instruction, Bool), Int)
fixNextInst is p = case is ! p of
  (Jmp x, b)  -> (is // [(p, (Nop x,b))], p')
  (Nop x, b)  -> (is // [(p, (Jmp x,b))], p')
  (Acc x, b)  -> fixNextInst is p'
  where p' = p+1

executeProgram:: Array Int (Instruction, Bool) -> Maybe Int
executeProgram is = executeProgram' is 0 0 size
  where size = 1 + (snd $ bounds is)

executeProgram':: Array Int (Instruction, Bool) -> Int -> Int -> Int -> Maybe Int
executeProgram' is p acc end
  | p > end || p < 0  = Nothing
  | p == end          = Just acc
  | loop              = Nothing
  | otherwise         = executeProgram' is' (nextPos i p) (modAcc i acc) end
  where (i, loop) = is ! p
        is' = is // [(p, (i,True))]
