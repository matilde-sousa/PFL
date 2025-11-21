-- a)
myinsert :: Ord a => a -> [a] -> [a]
myinsert n [] = [n]
myinsert n (x : xs) = 
    if n > x then x : (myinsert n xs)
    else n : (x : xs)

-- b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort [x] = [x]
isort (x : xs) = myinsert x (isort xs)
-- Recursive case:
-- 1. recursively sort the tail (isort xs)
-- 2. insert the head (x) into the correctly sorted tail (myinsert x)