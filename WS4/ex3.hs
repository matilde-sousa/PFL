
aux :: Integer -> [Integer]
aux n = [2^i * 3^j * 5^k | i <- [0..n], j <- [0..n], k <-[0..n] , i + j + k == n]

hammingNumbers :: [Integer]
hammingNumbers = concat [aux n | n <- [0..]]

-- 4.4
-- a)

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) 
    | x < y = x : merge xs (y : ys)
    | y > x = y : merge ys (x : xs)
    | otherwise = x : merge xs ys

-- b)
merge3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
merge3 xs ys zs = merge xs (merge ys zs)

hamming :: [Integer]
hamming = 1 : merge3 (map (2*) hamming) (map (3*) hamming ) (map (5*) hamming )