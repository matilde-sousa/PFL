propDivs :: Integer -> [Integer]
propDivs n = [ d | d <- [1..n-1], n `mod` d == 0]


isPrime :: Integer -> Bool
isPrime n