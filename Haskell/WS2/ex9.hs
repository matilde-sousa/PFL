propDivs :: Integer -> [Integer]
propDivs n = [ d | d <- [1..n-1], n `mod` d == 0]

perfects :: Integer -> [Integer] 
perfects n = [p | p <- [1..n], sum (propDivs p) == p]