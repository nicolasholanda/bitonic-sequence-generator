{-|
Module      : BitonicSequence
Description : Core algorithm for generating bitonic sequences
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module provides the core algorithm for generating bitonic sequences.
A bitonic sequence is a sequence that first increases and then decreases.
-}

-- The "module" keyword defines a namespace (like a Java package)
-- We export only "bitonicArray" - other modules can only use this function
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

-- Type signature (like Java: public List<Integer> bitonicArray(int n, int l, int r))
-- Read as: "bitonicArray takes 3 Ints and returns a list of Ints"
-- :: means "has type"
-- -> separates parameters and return type
bitonicArray :: Int -> Int -> Int -> [Int]

-- Function definition with 3 parameters: n, l, r
-- In Haskell, we use pattern matching and guards instead of if/else
bitonicArray n l r
    -- Guard (like if statement): checks condition before the =
    -- | means "when this condition is true"
    -- If impossible to create sequence, return list with single element [-1]
    | n > (r - l) * 2 + 1 = [-1]
    
    -- Special case: if n is 1, just return the max value
    | n == 1 = [r]
    
    -- Otherwise, build the bitonic sequence
    -- "otherwise" is just a synonym for "True" - the default case
    | otherwise = 
        -- "let" introduces local variables (like declaring variables in a method)
        -- These variables are only visible inside this "let...in" block
        let 
            -- Calculate midpoint: (n + 1) divided by 2 (integer division)
            -- `div` is the function, backticks make it infix (between arguments)
            -- Same as: div (n + 1) 2
            mid = (n + 1) `div` 2 
            
            -- Build the ascending part (going up)
            -- "if...then...else" is an expression (returns a value)
            ascending = if n <= 5 
                       -- For small n, start close to max: [r-1, r]
                       then [r - 1, r]
                       -- For larger n, start from l: [l, l+1, l+2, ..., l+mid-1]
                       -- [l .. l + mid - 1] is range syntax (like Python's range)
                       else [l .. l + mid - 1]
            
            -- Figure out where the descending part starts
            descendingStart = if n <= 5 
                             -- For n=5 with r=10: starts at 9
                             then r - 1
                             -- For larger n: starts at r-1
                             -- Example: if r=5, starts at 4
                             else r - 1
            
            -- How many elements in the descending part?
            -- Total length (n) minus ascending part length
            -- "length" gets the size of a list
            descendingCount = n - length ascending
            
            -- Build the descending part (going down)
            -- [descendingStart, descendingStart - 1 .. l] means:
            -- "start at descendingStart, subtract 1 each time, until we reach l"
            -- Example: [9, 8, 7, 6, ...] down to l
            -- "take" limits to first descendingCount elements
            descending = take descendingCount [descendingStart, descendingStart - 1 .. l]
            
        -- "in" ends the "let" block and specifies what to return
        -- ++ is list concatenation operator (like + for strings in Java)
        -- Combines ascending [1,2,3] and descending [3,2,1] -> [1,2,3,3,2,1]
        in ascending ++ descending
