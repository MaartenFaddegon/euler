import Data.List.Ordered

divides x y = y `mod` x == 0
dividedBy x y = y `divides` x

solution1 = sum [x | x <- [1..999], 3 `divides` x || 5 `divides` x]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

solution2 = sum $ filter even $ takeWhile (< 4000000) fibs

-- the Sieve of Eratosthenes
primesUpTo n = 2 : sieve [3,5..n]
    where sieve (p:xs)
            | p*p > n   = p : xs
            | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

primes = 2 : primes'
  where primes' = sieve [3,5..] 9 primes'
        sieve (x:xs) q ps@ ~(p:t)
            | x < q     = x : sieve xs q ps
            | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

-- Note that using primes rather than primesUpTo allows laziness
-- to do its jobs and stop as soon as n is factored.
factorize n = factorize' n $ primes
    where factorize' n ps
            | n == 1    = []
            | otherwise = let ps' = dropWhile (not . (n `dividedBy`)) ps
                          in  case ps' of
                          (p:ps'') -> p : (factorize' (n `div` p) ps')
                          ps''     -> ps''

solution3 = last $ factorize 600851475143
