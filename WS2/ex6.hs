-- a)

--short :: [a] -> Bool
--short xs = if length (xs) < 3 then True else False

-- b)

short :: [a] -> Bool
short [x] = True
short [x,y] = True
short xs  | otherwise = False