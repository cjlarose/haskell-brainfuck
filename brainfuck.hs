import Data.Array

collectChars :: String -> String -> String
collectChars = filter . flip elem

type Program = [Statement]
data Statement = Command Char | Loop [Statement]

-- given a list of tokens, build a Program
-- parse :: String -> Program
-- parse ('[':rest) = []
-- parse (token:rest) = []

-- getNextStatement :: Int -> String -> (Statement, String)
-- getNextStatement d ('[':rest) = (Loop [], rest)
--   foldl f [] 
--   where xs = getNextStatement (d + 1) rest
-- getNextStatement d (token:rest) = (Command token, rest)

parse :: String -> Program
parse = parse' 0 []

-- bracket depth, program, text
parse' :: Int -> Program -> String -> Program
parse' 0 prog "" = prog
parse' _ _ ""    = error "Failed to parse program"
parse' d prog (c:cs)
  | c `elem` "><+=.," = parse' d (Command c:prog) cs
  | c == '['          = parse' (d + 1) [] 

-- type BFState = (Int, Int, Array Integer Integer)

-- executeStatement :: BFState -> Char -> BFState
-- executeStatement (i, j, tape) command = (i + 1, j, tape)

-- program state = (program counter, data counter, tape)
-- fold 
-- given a state and a char, return a new state
-- go until eof

main = do
       program <- getContents
       putStrLn $ (collectChars "><+-.,[]" program)
