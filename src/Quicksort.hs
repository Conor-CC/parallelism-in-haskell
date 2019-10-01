module Quicksort where

import Control.Parallel

-- quicksort takes the head of the list as a pivot, moves it to the middle
-- such that the left most elements are smaller than the pivot and the rightmost
-- are larger. Quicksort will then repeat this process on every sub list generated
-- until dividing up the list results in a pivot with two empty lists either side
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []     -- Base case
quicksort (x:xs) =    -- Workhorse case
    let smallList = quicksort [a | a <- xs, a <= x]
        bigList = quicksort [a | a <- xs, a > x]
    in  smallList ++ [x] ++ bigList

paraQuicksort :: (Ord a) => [a] -> [a]
paraQuicksort [] = []     -- Base case
paraQuicksort (x:xs) = par bigList (pseq smallList (smallList ++ x:bigList))
            where bigList = quicksort [a | a <- xs, a > x]
                  smallList = quicksort [a | a <- xs, a <= x]
