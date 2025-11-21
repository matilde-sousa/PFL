bitobtainer :: Int -> Int
bitobtainer 0 = 0
bitobtainer x =
    if x `mod` 2 == 0 then 0
    else 1


toBitsR :: Int -> [Int]
toBitsR 0 = []
toBitsR x = bitobtainer x : (toBitsR (x `div` 2))

toBits :: Int -> [Int]
toBits x = reverse (toBitsR x)
