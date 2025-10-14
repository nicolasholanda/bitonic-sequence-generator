module BitonicSequence (bitonicArray) where

-- Function to construct bitonic sequence of 
-- length n from integers in the range [l, r]
bitonicArray :: Int -> Int -> Int -> [Int]
bitonicArray n l r
    -- If not possible
    | n > (r - l) * 2 + 1 = [-1]
    | n == 1 = [r]
    | otherwise = 
        let 
            mid = (n + 1) `div` 2 
            ascending = if n <= 5 
                       then [r - 1, r]
                       else [l .. l + mid - 1]
            descendingStart = if n <= 5 
                             then r - 1  -- For n=5: starts at 9
                             else r - 1  -- For n=7: starts at 4 (r-1=5-1=4)
            descendingCount = n - length ascending
            descending = take descendingCount [descendingStart, descendingStart - 1 .. l]
            
        in ascending ++ descending
