{-|
Module      : BitonicSequence
Description : Core algorithm for generating bitonic sequences
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module provides the core algorithm for generating bitonic sequences.
A bitonic sequence is a sequence that first increases and then decreases.
-}
module BitonicSequence (bitonicArray) where

{-| 
Generate a bitonic sequence of length n from integers in the range [l, r].

A bitonic sequence first increases to a maximum value, then decreases.
The first element should be as large as possible within the constraints.

Examples:

>>> bitonicArray 5 3 10
[9,10,9,8,7]

>>> bitonicArray 7 1 5
[1,2,3,4,5,4,3]

>>> bitonicArray 20 1 5
[-1]

Returns @[-1]@ if it's impossible to construct a valid sequence with the given parameters.
This happens when @n > (r - l) * 2 + 1@.
-}
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
            descendingStart = r - 1
            descendingCount = n - length ascending
            descending = take descendingCount [descendingStart, descendingStart - 1 .. l]
            
        in ascending ++ descending
