max_, min_ :: Ord a => a -> a -> a
max x y = if x>=y then x else y
min x y = if x<=y then x else y

--max3, min3 :: Ord a => a -> a -> a -> a
--max3 x y z = if (x>=y && x>=z) then x 
  --          else if (y>=z) then y
    --        else z 
--min3 x y z = if (x<=y && x<=z) then x 
  --          else if (y<=z) then y
    --        else z 

-- max3 and min3 using the associativity of max and min 
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z = max_ (max_ x y) z
min3 x y z = min_ (min_ x y) z