divides x y = y `mod` x == 0

solution1 = sum [x | x <- [1..999], 3 `divides` x || 5 `divides` x]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

solution2 = sum $ filter even $ takeWhile (< 4000000) fibs
