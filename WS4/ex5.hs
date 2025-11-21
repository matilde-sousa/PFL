import Data.Char (ord, chr)
import Test.QuickCheck (quickCheck, Property, (==>))

-- Logic:
-- 1. Convert the character 'c' and its 'base' to their ASCII integer values (ord).
-- 2. Calculate the zero-based index of 'c' within its letter block: (ord c - ord base).
-- 3. Add 13 to the index, and take the remainder (modulo) with 26 (the number of letters).
-- 4. Add the base offset back to get the ASCII value of the rotated character.
-- 5. Convert the final integer back to a character (chr).
rotate :: Char -> Char -> Char
rotate base c = chr $ (ord base) + ((ord c - ord base + 13) `mod` 26)

rot13 :: Char -> Char
rot13 c 
    | c >= 'A' && c <= 'Z' = rotate 'A' c
    | c >= 'a' && c <= 'z' = rotate 'a' c
    | otherwise  = c

prop_rot13_is_inverse :: String -> Bool
prop_rot13_is_inverse s = rot13 (rot13 's') == 's'

main :: IO ()
main = do
    putStrLn "--- Running QuickCheck Test (Pure Core) ---"
    quickCheck prop_rot13_is_inverse
    putStrLn "------------------------------------------"
    putStrLn "Enter text to ROT13 (press to proceed): "
    line <- getLine
    let encryptedLine = map rot13 line
    putStrLn encryptedLine
    return ()