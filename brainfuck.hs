import Data.Array

collectChars :: String -> String -> String
collectChars = filter . flip elem

main = do
       program <- getContents
       putStrLn $ (collectChars "><+-.,[]" program)
