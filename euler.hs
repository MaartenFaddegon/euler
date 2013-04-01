import Data.List.Ordered
import System.Environment
import Data.Char

divides x y = y `mod` x == 0
dividedBy x y = y `divides` x

solution1 = sum [x | x <- [1..999], 3 `divides` x || 5 `divides` x]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

solution2 = sum $ filter even $ takeWhile (< 4000000) fibs

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
factorize n = factorize' n $ primes
    where factorize' n ps
            | n == 1    = []
            | otherwise = let ps' = dropWhile (not . (n `dividedBy`)) ps
                          in  case ps' of
                          (p:ps'') -> p : (factorize' (n `div` p) ps')
                          ps''     -> ps''

solution3 = last $ factorize 600851475143

-- Yes, we could compare only the first half of the strings. But determining
-- how long the string actually is, probably needs equally much time as just
-- comparing a bit more.
palindromic x = s == s'
    where s  = show x
          s' = reverse s

solution4 = maximum [x*y | x <- [1..999], y <- [1..999], palindromic (x*y)]

listDivides xs y = foldr (\x accum -> accum && x `divides` y) True xs

solution5 = head $ filter (listDivides [1..20]) [1..]

sumOfSquares n = sum [x^2 | x <- [1..n]]

squareOfSum n = (sum [1..n]) ^ 2

solution6 = (squareOfSum 100) - (sumOfSquares 100)

solution7 = primes !! (10001-1)

digitsToInts cs = foldr (\c is-> (digitToInt c):is) [] cs

consecutiveSubsets :: Int -> [Int] -> [[Int]]
consecutiveSubsets n xs
    | length xs >= n = (take n xs) : (consecutiveSubsets n (tail xs))
    | otherwise      = []

solution8 = maximum $ map product $ consecutiveSubsets 5 $ digitsToInts 
          $  "73167176531330624919225119674426574742355349194934"
          ++ "96983520312774506326239578318016984801869478851843"
          ++ "85861560789112949495459501737958331952853208805511"
          ++ "12540698747158523863050715693290963295227443043557"
          ++ "66896648950445244523161731856403098711121722383113"
          ++ "62229893423380308135336276614282806444486645238749"
          ++ "30358907296290491560440772390713810515859307960866"
          ++ "70172427121883998797908792274921901699720888093776"
          ++ "65727333001053367881220235421809751254540594752243"
          ++ "52584907711670556013604839586446706324415722155397"
          ++ "53697817977846174064955149290862569321978468622482"
          ++ "83972241375657056057490261407972968652414535100474"
          ++ "82166370484403199890008895243450658541227588666881"
          ++ "16427171479924442928230863465674813919123162824586"
          ++ "17866458359124566529476545682848912883142607690042"
          ++ "24219022671055626321111109370544217506941658960408"
          ++ "07198403850962455444362981230987879927244284909188"
          ++ "84580156166097919133875499200524063689912560717606"
          ++ "05886116467109405077541002256983155200055935729725"
          ++ "71636269561882670428252483600823257530420752963450"

interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

-- Nick Lord "A uniform construction of some infinite coprime sequences"
coprimes = [(2,1),(3,1)] ++ interleave (gen (2,1)) (gen (3,1))
    where gen (m,n) = let branch1 = (2*m-n, m)
                          branch2 = (2*m+n, m)
                          branch3 = (  m+2*n,n)
                      in  [branch1, branch2, branch3] 
                          ++ (gen branch1) ++ (gen branch2) ++ (gen branch3)

-- Euclid's formula
primitivePythagoreanTriples = map ppt coprimes
    where ppt (m,n) = (m^2 - n^2, 2*m*n, m^2 + n^2)

pythagoreanTripleVariations (a,b,c) = [(k*a,k*b,k*c) | k <- [1..]]


isPT (a,b,c) = a^2 + b^2 == c^2

testPPT = [] == (filter (not . isPT) $ take 100 primitivePythagoreanTriples)

testPT = [] == (filter (not . isPT) $ pts)
    where pts  = foldl (++) [] $ map ((take 5) . pythagoreanTripleVariations) ppts
          ppts = take 10 primitivePythagoreanTriples

solution9 = case pt1000 of
                []           -> -1
                ((a,b,c):ts) -> a*b*c
    where pt1000 = dropWhile (\(a,b,c) -> a + b + c /= 1000) $ reverse pts
          pts    = foldl (++) [] $ map (takeW . pythagoreanTripleVariations) ppts
          ppts   = takeW primitivePythagoreanTriples
          takeW  = takeWhile (\(a,b,c) -> a + b + c <= 1000)


main = do args <- getArgs
          putStr $ solve args

solve []                  = ""
solve ("-solution1":args) = show solution1 ++ "\n" ++ solve args
solve ("-solution2":args) = show solution2 ++ "\n" ++ solve args
solve ("-solution3":args) = show solution3 ++ "\n" ++ solve args
solve ("-solution4":args) = show solution4 ++ "\n" ++ solve args
solve ("-solution5":args) = show solution5 ++ "\n" ++ solve args
solve ("-solution6":args) = show solution6 ++ "\n" ++ solve args
solve ("-solution7":args) = show solution7 ++ "\n" ++ solve args
solve ("-solution8":args) = show solution8 ++ "\n" ++ solve args
solve ("-solution9":args) = show solution9 ++ "\n" ++ solve args
solve _                   = "usage: ./euler -solution<num>\n"
