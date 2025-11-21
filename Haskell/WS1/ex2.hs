leftHalf :: [a] -> [a] 
leftHalf xs = take h xs
    where h = length xs `div`  2

rightHalf :: [a] -> [a]
rightHalf xs = drop h xs
    where h =  length xs `div` 2