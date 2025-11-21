calcPi1 :: Int -> Double 
calcPi1 n = sum(take n ts)
    where ns = cycle [4, -4]  -- numeradores: 4, -4, 4, -4 ...
          ds = [1,3..] -- denominadores: 1, 3, 5, 7 ...
          ts = zipWith(/) ns ds -- combina numeradores e denominadores

calcPi2 :: Int -> Double
calcPi2 n = sum( take n ts)
    where ns = cycle [4, -4]
          ds = [2*3*4, 4*5*6..]
          ts = 3 : zipWith(/) ns (map fromIntegral ds)