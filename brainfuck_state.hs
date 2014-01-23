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
executeCommand :: BFState -> Char -> (Maybe Int, BFState)
executeCommand s '>' = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1, 
    tape = tape s, 
    dataPointer = dataPointer s + 1})
executeCommand s '<' = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = tape s, 
    dataPointer = dataPointer s - 1})
executeCommand s '+' = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = newTape, 
    dataPointer = dataPointer s})
    where newTape = tape s // [(dataPointer s, tape s ! dataPointer s + 1)]
executeCommand s '-' = (Nothing, BFState {
    program = program s, 
    programCounter = programCounter s + 1,
    tape = newTape, 
    dataPointer = dataPointer s})
    where newTape = tape s // [(dataPointer s, tape s ! dataPointer s - 1)]

main = do
       program <- getContents
       putStrLn $ (collectChars "><+-.,[]" program)
