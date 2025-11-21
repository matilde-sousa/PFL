-- Type signature: nub takes a list of elements that can be compared for equality
-- and returns a list with duplicates removed (keeping first occurrences).
nub :: (Eq a) => [a] -> [a]
-- Start the algorithm by calling the helper 'go' with the input list and
-- an empty 'seen' accumulator (which tracks elements already emitted).
nub xs = go xs []
  where
    -- If there are no more elements to process, return an empty list.
    go [] _ = []
    -- Match the list into head 'x' and tail 'xs', with the current 'seen' list.
    go (x : xs) seen
      -- If 'x' is already in 'seen', we skip it and continue with the tail.
      | x `elem` seen = go xs seen
      -- Otherwise, emit 'x' (keep first occurrence) and add it to 'seen'
      -- before continuing; this preserves the input order for kept elements.
      | otherwise = x : go xs (x : seen)
