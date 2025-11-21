import System.Environment (getArgs)
import System.Exit (die)
import System.IO (getContents)

type AWord = String
type Line = [AWord]
type Paragraph = [Line]

takeLine :: Int -> [AWord] -> (Line, [AWord])
takeLine _ [] = ([], [])
takeLine width (w:ws) = go [w] (length w) ws
    where 
       go line currentLen [] = (line, [])
       go line currentLen (x : xs)
        | currentLen + 1 + length x <= width = go (line ++ [x]) (currentLen + 1 + length x) xs
        | otherwise = (line, x:xs)

fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords width words =
    let (line, rest) = takeLine width words
    in line : fillWords width rest

main :: IO ()
main = do
    args <- getArgs
    width <- case args of
        (a:_) -> pure (read a)     -- take first argument
        []    -> die "Usage: Wrap <width>"
    text <- getContents
    let wordList  = words text
        paragraph = fillWords width wordList
        formatted = unlines (map unwords paragraph)
    putStr formatted