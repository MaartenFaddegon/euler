module Liblist where
import Data.Char
import Data.List

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

powerset :: [a] -> [[a]]
powerset = subsequences

sumOfDigits i = sum $ map digitToInt $ show i

zipfold f (a:as) (b1:b2:bs) = f a b1 b2 : zipfold f as (b2:bs)
zipfold _  _      _         = []

combinations :: [a] -> [(a,a)]
combinations [] = []
combinations (x:xs) = combinationsWith x xs ++ combinations xs

combinationsWith :: a -> [a] -> [(a,a)]
combinationsWith x xs  = zip (cycle [x]) (x:xs)
