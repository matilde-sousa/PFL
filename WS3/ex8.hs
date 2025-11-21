fromBitsAccumulator :: [Int] -> Int -> Int
-- 1. Base Case: The list of bits is empty. Return the final accumulated value.
fromBitsAccumulator [] acc = acc 
-- 2. Recursive Case: Process the head bit and update the accumulator.
fromBitsAccumulator (x : xs) acc = fromBitsAccumulator xs new_acc
    where 
        -- The left-shift operation (acc * 2) makes room for the new bit (x).
        new_acc = (acc * 2) + x

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits bits = fromBitsAccumulator bits 0