module Liblist where
import Data.Char

-- Yes, we could compare only the first half of the strings. But determining
-- how long the string actually is, probably needs equally much time as just
-- comparing a bit more.
palindromic x = s == s'
    where s  = show x
          s' = reverse s

digitsToInts cs = foldr (\c is-> (digitToInt c):is) [] cs

consecutiveSubsets :: Int -> [Int] -> [[Int]]
consecutiveSubsets n xs
    | length xs >= n = (take n xs) : (consecutiveSubsets n (tail xs))
    | otherwise      = []


interleave (x:xs) (y:ys) = x:y:(interleave xs ys)