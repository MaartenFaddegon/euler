import Data.List.Ordered (minus)
import System.Environment
import Data.Char
import Debug.Trace
import Data.List

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
primeFactors n = primeFactors' n $ primes
    where primeFactors' n ps
            | n == 1    = []
            | otherwise = let ps' = dropWhile (not . (n `dividedBy`)) ps
                          in  case ps' of
                          (p:ps'') -> p : (primeFactors' (n `div` p) ps')
                          ps''     -> ps'

test3     = primeFactors 600851475143
solution3 = last $ primeFactors 600851475143

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

solution10 = sum $ takeWhile (< 2000000) primes

collength matrix = length matrix
rowlength matrix
    | matrix == [] = 0
    | otherwise    = length (matrix !! 0)

(!!!) matrix (m,n) = (matrix !! m) !! n

col matrix n = foldr (\row col -> (row !! n):col) [] matrix

diag matrix (m,n) = [matrix !!! (x+m,x+n) | x <- [0..min k l], x+m < k, x+n < l]
    where k = rowlength matrix
          l = collength matrix

-- reverse rows
mirror = map (reverse)

-- all top-left to bottom right diags
diags matrix =  [diag matrix (0,n) | n <- [0..l]] 
             ++ [diag matrix (m,0) | m <- [1..k]] 
    where k = rowlength matrix - 1
          l = collength matrix - 1

-- cols become rows
rotate90 matrix = [col matrix n | n <- [0..length (matrix!!0)-1]]

solution11 = maximum $ map product
           $ subsets matrix ++ subsets matrix' ++ subsets matrix'' ++ subsets matrix'''
    where subsets   = foldr (\row sets -> sets ++ consecutiveSubsets 4 row) []
          matrix''' = diags (mirror matrix)
          matrix''  = diags matrix
          matrix'   = rotate90 matrix
          matrix    = [[08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08]
                      ,[49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00]
                      ,[81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65]
                      ,[52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91]
                      ,[22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80]
                      ,[24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50]
                      ,[32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70]
                      ,[67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21]
                      ,[24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72]
                      ,[21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95]
                      ,[78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92]
                      ,[16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57]
                      ,[86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58]
                      ,[19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40]
                      ,[04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66]
                      ,[88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69]
                      ,[04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36]
                      ,[20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16]
                      ,[20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54]
                      ,[01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]
                      ]

triangularNumber n = (n * (n+1)) `div` 2

triangularNumbers = [triangularNumber n | n <- [1..]]

powerset :: [a] -> [[a]]
powerset = subsequences

-- find all integers that divise n
factors n  = map (\ps -> foldr (*) 1 ps) pss
    where pss = nub $ powerset $ primeFactors n

test12 = foldl (\acc (n,fs) -> acc ++ show n ++ "\t" ++ show fs ++ "\n" ) ""
       $ map (\n -> (n, factors n)) $ take 7 triangularNumbers
solution12 = (\(n,_)-> n) $ head $ dropWhile (\(_,f) -> f<500) 
                                 $ map (\n -> (n, (length . factors) n)) triangularNumbers

main = do args <- getArgs
          putStr $ solve args

solve []                   = ""
solve ("-solution1":args)  = show solution1  ++ "\n" ++ solve args
solve ("-solution2":args)  = show solution2  ++ "\n" ++ solve args
solve ("-test3":args)      = show test3      ++ "\n" ++ solve args
solve ("-solution3":args)  = show solution3  ++ "\n" ++ solve args
solve ("-solution4":args)  = show solution4  ++ "\n" ++ solve args
solve ("-solution5":args)  = show solution5  ++ "\n" ++ solve args
solve ("-solution6":args)  = show solution6  ++ "\n" ++ solve args
solve ("-solution7":args)  = show solution7  ++ "\n" ++ solve args
solve ("-solution8":args)  = show solution8  ++ "\n" ++ solve args
solve ("-solution9":args)  = show solution9  ++ "\n" ++ solve args
solve ("-solution10":args) = show solution10 ++ "\n" ++ solve args
solve ("-solution11":args) = show solution11 ++ "\n" ++ solve args
solve ("-test12":args)     = test12          ++ "\n" ++ solve args
solve ("-solution12":args) = show solution12 ++ "\n" ++ solve args
solve _                    = "usage: ./euler -solution<num>\n"
