import Distribution.Compat.Lens (_1)
import Distribution.Simple.Utils (xargs)
-- a)

myand :: [Bool] -> Bool
myand  [] = True
myand (False : _ ) = False
myand (_ : xs) = myand xs


-- b)
myor :: [Bool] -> Bool
myor [] = False
myor (True : _) = True
myor (_ : xs) = myor xs


-- c)
myconcat :: [[a]] -> [a] 
myconcat [] = []
myconcat (xs:xss) = xs ++ myconcat xss

-- d)
myreplicate :: Int -> a -> [a] 
myreplicate 0 _ = []
myreplicate x a = a : myreplicate (x-1) a

-- e)
myindex :: [a] -> Int -> a
myindex (x : _) 0 = x -- base case: found desired element
myindex (_ : xs) n = myindex xs (n-1) -- recursive case: decrement index and check the tail


--f)
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem a (x : _) | a == x = True
myelem x (_ : xs) = myelem x xs