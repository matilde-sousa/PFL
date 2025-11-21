{- 
median :: Ord a => a -> a -> a -> a
median x y z 
    | (x <= y && y <= z) || (z<=y && y<=x) = y
    | (y<= x && x<=z) || (z<=x && x<=y) = x
    | (x<= z && z<=y) || (y<=z && z<=y) = z
-}

median :: (Ord a, Num a )=> a -> a -> a -> a
median x y z = x + y +z - maximum[x,y,z] - minimum[x,y,z]