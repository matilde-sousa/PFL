second :: [a] -> a
second xs = head (tail xs)

lastE :: [w] -> w
lastE l = head (reverse l) 

initL :: [Int] -> [Int]
initL r = reverse (tail(reverse r))

middle :: [Int] -> Int
middle m = head (drop half m )
    where half = length m `div` 2

checkPalindrome :: String -> Bool
checkPalindrome p = p == reverse p