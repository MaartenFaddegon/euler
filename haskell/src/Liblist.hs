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

breaks :: (a->Bool) -> [a] -> [[a]]
breaks p xs = case break p xs of
 (pre,[])  -> [pre]
 (pre,rem) -> pre : breaks p (tail rem)


-- There are n! permutations for a list of n elements. If we pick one element,
-- there are (n-1)! remaining permutations starting with that element. We can
-- use this to enumerate all permutations and to generate permutation i:
-- permutations starting with the first element have index 0 <= i < (n-1)!, 
-- the second element (n-1)! <= i < 2*(n-1)!, etcetera.
--
-- Also see the 2nd algorithm mentioned in: http://stackoverflow.com/a/362714/2424911
--
permutation :: [a] -> Int -> [a]
permutation xs i = permutation' xs (i-1)
permutation' [] i  | i == 0 = []
permutation' [] _           = error "permutation: index too large"
permutation' xs i = x : permutation' xs' j
  where n = length xs
        p = (i `div` (fac $ n-1)) `mod` n
        j = i `mod` (fac $ n-1)
        (x,xs') = get p xs

        fac :: Int -> Int
        fac n = product [1..n]

        get :: Int -> [a] -> (a,[a])
        get n xs     | n < 0 = error "permutation.get: negative index"
        get _ []             = error "permutation.get: index too large"
        get 0 (x:xs)         = (x,xs)
        get n (x:xs)         = let (y,ys) = get (n-1) xs in (y,x:ys)
