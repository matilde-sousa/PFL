import Text.Read (Lexeme(String))
type Dict = [String]

readDict :: IO Dict
readDict = do
    txt <- readFile "/usr/share/dict/words"
    return (words txt)

reverseOn = "\ESC[7m"
reverseOff = "\ESC[0m"

checkWord :: Dict -> String -> String
checkWord dict word = 
    if word `elem` dict
    then word 
    else reverseOn ++ word ++ reverseOff

spellCheck :: Dict -> String -> String
spellCheck dict text = unlines checkedLines
    where
        -- 1. split the entire input text into a list of individual lines: [String]
        textLines = lines text
        -- 2. Map helper function, checkline, over every line in textLines
        checkedLines = map (checkLine dict) textLines
    
        checkLine :: Dict -> String -> String
        checkLine d line =
            let lineWords = words line
                checkedWords = map (checkWord d) lineWords
            in unwords checkedWords
    

main :: IO()
main = do
    -- 1. Call readDict to load the dictionary from the file
    dictionary <- readDict
    
    -- PART OF ANOTHER EXERCISE
    -- 2. Use length to count the number of words in the list
    -- let wordCount = length dictionary
    -- 3. Print result
    --putStrLn ("Successfully loaded the dictionary.")
    --putStrLn ("Total words in dictionary: " ++ show wordCount)

    -- Read all input from standard input (stdin) until EOF is reached.
    inputText <- getContents

    -- Run the pure spellCheck function using the dictionary and the input text.
    let checkedText = spellCheck dictionary inputText

    putStr checkedText