-- using conditional expression if..then..else

--safetail :: [a] -> [a]
--safetail xs = if null xs then [] else tail(xs)

--safetail :: [a] -> [a]
--safetail xs |  null xs = []
 --           | otherwise = tail xs

safetail :: [a] -> [a]
safetail [] = []        -- if input is empty list, returns empty list
safetail (_:xs) = xs    -- matches any list of the form (x:xs), that's a head element x and a tail xs ; we don't need x, so we use _ as a wildcard to ignore it
