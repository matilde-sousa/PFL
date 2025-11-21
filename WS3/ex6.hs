
-- a)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = 
    if x <= y then x : (merge xs (y : ys))
    else y : (merge (x : xs) ys)

-- b)
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort list1) (msort list2)
    where
        half = length xs `div` 2
        (list1, list2) = splitAt half xs