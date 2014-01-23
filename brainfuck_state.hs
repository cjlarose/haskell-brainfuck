import Data.Array

collectChars :: String -> String -> String
collectChars = filter . flip elem

data BFState =
    BFState {program :: [Int],
             programCounter :: Int,
             tape :: Array Int Int,
             dataPointer :: Int}
    deriving (Eq, Show, Read)

-- read the instruction of the bfstate at its program counter, feed it to a fn
-- that'll return a new BFState
executeCommand :: BFState -> Char -> Int -> (Maybe Int, BFState)
executeCommand s '>' _ = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1, 
    tape = tape s, 
    dataPointer = dataPointer s + 1})
executeCommand s '<' _ = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = tape s, 
    dataPointer = dataPointer s - 1})
executeCommand s '+' _ = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = newTape, 
    dataPointer = dataPointer s})
    where newTape = tape s // [(dataPointer s, tape s ! dataPointer s + 1)]
executeCommand s '-' _ = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = newTape, 
    dataPointer = dataPointer s})
    where newTape = tape s // [(dataPointer s, tape s ! dataPointer s - 1)]
executeCommand s '.' _ = (Just value, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = tape s, 
    dataPointer = dataPointer s})
    where value = tape s ! dataPointer s
executeCommand s ',' c = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = newTape, 
    dataPointer = dataPointer s})
    where newTape = tape s // [(dataPointer s, c)]

main = do
       program <- getContents
       putStrLn $ (collectChars "><+-.,[]" program)
