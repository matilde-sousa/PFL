-- a)

leastDiv :: Integer -> Integer
leastDiv n = go n 2
    where
        go n d
            | n `mod` d == 0 = d -- base case: div found, return d
            | d * d > n =  n -- base case: limit reached, return n
            | otherwise = go n (d + 1) -- recursive step: update state (d+1)

-- b)

isPrimeFast :: Integer -> Bool
isPrimeFast n 
    | n <= 1 = False
    | leastDiv n == n = True
    | otherwise = False

isPrimeFast2 :: Integer -> Bool
isPrimeFast2 n = n > 1 && (leastDiv n == n)