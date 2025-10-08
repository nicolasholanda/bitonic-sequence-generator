module BitonicSequence (bitonicArray) where

-- Function to construct bitonic sequence of 
-- length n from integers in the range [l, r]
bitonicArray :: Int -> Int -> Int -> [Int]
bitonicArray n l r
    -- If not possible
    | n > (r - l) * 2 + 1 = [-1]
    | otherwise = buildSequence n l r

buildSequence :: Int -> Int -> Int -> [Int]
buildSequence n l r = 
    let -- Start with r-1
        initial = [r - 1]
        -- Add decreasing part: from r down to l
        decreasingPart = addDecreasing initial [r, r-1 .. l] n
        -- Add increasing part: from r-2 down to l (prepend)
        finalSeq = addIncreasing decreasingPart [r - 2, r - 3 .. l] n
    in finalSeq

-- Helper function to add decreasing part (append to end)
addDecreasing :: [Int] -> [Int] -> Int -> [Int]
addDecreasing current [] _ = current
addDecreasing current _ n | length current >= n = current
addDecreasing current (x:xs) n 
    | length current >= n = current
    | otherwise = addDecreasing (current ++ [x]) xs n

-- Helper function to add increasing part (prepend to beginning)
addIncreasing :: [Int] -> [Int] -> Int -> [Int]
addIncreasing current [] _ = current
addIncreasing current _ n | length current >= n = current
addIncreasing current (x:xs) n 
    | length current >= n = current
    | otherwise = addIncreasing (x : current) xs n
