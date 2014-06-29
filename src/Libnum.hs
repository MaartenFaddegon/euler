module Libnum where
import Data.List.Ordered (minus)
import Liblist
import Data.List

divides x y    = y `mod` x == 0
dividedBy x y  = y `divides` x
-- notDivides     = not . divides
notDivides x y    = not $ divides x y

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- The sieve of Eratosthenes
primesUpTo :: Integer -> [Integer]
primesUpTo n = 2 : sieve [3,5..n]
    where sieve (p:xs)
            | p^2 > n   = p : xs
            | otherwise = p : sieve (xs `minus` [p^2, p^2+2*p..])

-- The use of the '~' in the sieve pattern match delays the evaluation of the
-- value until the component part is actually used. 
primes :: [Integer]
primes = 2 : primes'
  where primes' = sieve [3,5..] 9 primes'
        sieve :: [Integer] -> Integer -> [Integer] -> [Integer]
        sieve (x:xs) q ps@ ~(p:t) 
            | x < q     = x : sieve xs q ps
            | otherwise = sieve (xs `minus` [q, q+2*p..]) (head t^2) t

-- Note that using primes rather than primesUpTo allows laziness
-- to do its jobs and stop as soon as n is factored.
primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n $ primes
    where primeFactors' n ps
            | n == 1    = []
            | otherwise = let ps' = dropWhile (not . (n `dividedBy`)) ps
                          in  case ps' of
                          (p:ps'') -> p : (primeFactors' (n `div` p) ps')
                          ps''     -> ps'

sumOfSquares :: Integer -> Integer
sumOfSquares n = sum [x^2 | x <- [1..n]]

squareOfSum :: Integer -> Integer
squareOfSum n = (sum [1..n]) ^ 2

-- Euclid's formula
primitivePythagoreanTriples :: [(Integer,Integer,Integer)]
primitivePythagoreanTriples = map ppt coprimes
    where ppt :: (Integer,Integer) -> (Integer,Integer,Integer)
          ppt (m,n) = (m^2 - n^2, 2*m*n, m^2 + n^2)

pythagoreanTripleVariations :: (Integer,Integer,Integer) -> [(Integer,Integer,Integer)]
pythagoreanTripleVariations (a,b,c) = [(k*a,k*b,k*c) | k <- [1..]]

-- isPT (a,b,c) = a^2 + b^2 == c^2
-- 
-- testPPT = [] == (filter (not . isPT) $ take 100 primitivePythagoreanTriples)
-- 
-- testPT = [] == (filter (not . isPT) $ pts)
--     where pts  = foldl (++) [] $ map ((take 5) . pythagoreanTripleVariations) ppts
--           ppts = take 10 primitivePythagoreanTriples


-- Nick Lord "A uniform construction of some infinite coprime sequences"
coprimes :: [(Integer,Integer)]
coprimes = [(2,1),(3,1)] ++ interleave (gen (2,1)) (gen (3,1))
    where gen (m,n) = let branch1 = (2*m-n, m)
                          branch2 = (2*m+n, m)
                          branch3 = (  m+2*n,n)
                      in  [branch1, branch2, branch3] 
                          ++ (gen branch1) ++ (gen branch2) ++ (gen branch3)

triangularNumber :: Integer -> Integer
triangularNumber n = (n * (n+1)) `div` 2

triangularNumbers :: [Integer]
triangularNumbers = [triangularNumber n | n <- [1..]]

-- find all integers that divise n
factors :: Integer -> [Integer]
factors n  = map (\ps -> foldr (*) 1 ps) pss
    where pss = nub $ powerset $ primeFactors n

collatzSequence :: Integer -> [Integer]
collatzSequence n
    | n == 1    = [1]
    | otherwise = n : collatzSequence (nextCollatz n)
        where nextCollatz n = if even n then n `div` 2
                              else 3 * n + 1


longestCollatzChain' :: (Integer,Int) -> Integer -> (Integer,Int)
longestCollatzChain' (m, l_m) n
    | l_n > l_m = (n ,l_n)
    | otherwise = (m, l_m)
    where l_n = length $ collatzSequence n

longestCollatzChain :: Integer -> (Integer,Int)
longestCollatzChain n = foldl longestCollatzChain' (1,1) [2..n]

factorial :: Integer -> Integer
factorial n = product [1..n]

numToWord i
    | i == 0     = ""      -- the empty 0 case is used to avoid making exceptions for 20, 30 etc.
    | i == 1     = "one"
    | i == 2     = "two"
    | i == 3     = "three"
    | i == 4     = "four"
    | i == 5     = "five"
    | i == 6     = "six"
    | i == 7     = "seven"
    | i == 8     = "eight"
    | i == 9     = "nine"
    | i == 10    = "ten"
    | i == 11    = "eleven"
    | i == 12    = "twelve"
    | i == 13    = "thirteen"
    | i == 15    = "fifteen"
    | i == 18    = "eighteen"
    | i <= 19    = numToWord (i-10) ++ "teen"
    | i <= 29    = "twenty"   ++ numToWord (i `mod` 10)
    | i <= 39    = "thirty"   ++ numToWord (i `mod` 10)
    | i <= 49    = "forty"    ++ numToWord (i `mod` 10)
    | i <= 59    = "fifty"    ++ numToWord (i `mod` 10)
    | i <= 69    = "sixty"    ++ numToWord (i `mod` 10)
    | i <= 79    = "seventy"  ++ numToWord (i `mod` 10)
    | i <= 89    = "eighty"   ++ numToWord (i `mod` 10)
    | i <= 99    = "ninety"   ++ numToWord (i `mod` 10)
    | i <= 999   =  numToWord  (i `div` 100)  ++ "hundred"  ++ (prefixAnd .numToWord) (i `mod` 100)
    | i <= 9999  =  numToWord  (i `div` 1000) ++ "thousand" ++ (prefixAnd . numToWord) (i `mod` 1000)
  where prefixAnd s
          | s == ""              = ""
          | "and" `isPrefixOf` s = s
          | otherwise            = "and" ++ s

sumWithMax i m n = i + (max m n)

mod1 i j
    | i > j     = 1
    | otherwise = i

properFactors i
    | length f <= 1 = []
    | otherwise     = init f
    where f = factors i

amicable a
    | b == 0    = Nothing
    | d b == a  = Just (a, b)
    | otherwise = Nothing
    where d i = case properFactors i of
                [] -> 0
                fs -> sum fs
          b = d a

amicablePairs []     = []
amicablePairs (a:as) = case amicable a of
    (Just pair) -> pair : amicablePairs as
    Nothing     -> amicablePairs as
