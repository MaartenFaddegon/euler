module Libnum where
import Data.List.Ordered (minus)
import Liblist

divides x y    = y `mod` x == 0
dividedBy x y  = y `divides` x
-- notDivides     = not . divides
notDivides x y    = not $ divides x y

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- The sieve of Eratosthenes
primesUpTo n = 2 : sieve [3,5..n]
    where sieve (p:xs)
            | p^2 > n   = p : xs
            | otherwise = p : sieve (xs `minus` [p^2, p^2+2*p..])

-- The use of the '~' in the sieve pattern match delays the evaluation of the
-- value until the component part is actually used. 
primes = 2 : primes'
  where primes' = sieve [3,5..] 9 primes'
        sieve (x:xs) q ps@ ~(p:t) 
            | x < q     = x : sieve xs q ps
            | otherwise = sieve (xs `minus` [q, q+2*p..]) (head t^2) t

-- Note that using primes rather than primesUpTo allows laziness
-- to do its jobs and stop as soon as n is factored.
primeFactors n = primeFactors' n $ primes
    where primeFactors' n ps
            | n == 1    = []
            | otherwise = let ps' = dropWhile (not . (n `dividedBy`)) ps
                          in  case ps' of
                          (p:ps'') -> p : (primeFactors' (n `div` p) ps')
                          ps''     -> ps'


sumOfSquares n = sum [x^2 | x <- [1..n]]

squareOfSum n = (sum [1..n]) ^ 2

-- Euclid's formula
primitivePythagoreanTriples = map ppt coprimes
    where ppt (m,n) = (m^2 - n^2, 2*m*n, m^2 + n^2)

pythagoreanTripleVariations (a,b,c) = [(k*a,k*b,k*c) | k <- [1..]]


-- isPT (a,b,c) = a^2 + b^2 == c^2
-- 
-- testPPT = [] == (filter (not . isPT) $ take 100 primitivePythagoreanTriples)
-- 
-- testPT = [] == (filter (not . isPT) $ pts)
--     where pts  = foldl (++) [] $ map ((take 5) . pythagoreanTripleVariations) ppts
--           ppts = take 10 primitivePythagoreanTriples


-- Nick Lord "A uniform construction of some infinite coprime sequences"
coprimes = [(2,1),(3,1)] ++ interleave (gen (2,1)) (gen (3,1))
    where gen (m,n) = let branch1 = (2*m-n, m)
                          branch2 = (2*m+n, m)
                          branch3 = (  m+2*n,n)
                      in  [branch1, branch2, branch3] 
                          ++ (gen branch1) ++ (gen branch2) ++ (gen branch3)
