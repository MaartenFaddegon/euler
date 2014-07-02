module Libtext where
import Data.Char
import Data.List

ordA :: Int
ordA = ord 'A'

-- A = 1, B = 2, ...
upperOrd :: Char -> Int
upperOrd c = 1 + ord c - ordA

upperCmp :: String -> String -> Ordering
upperCmp [] [] = EQ
upperCmp [] _  = LT
upperCmp _ []  = GT
upperCmp (c1:s1) (c2:s2) 
  = case compare (upperOrd c1) (upperOrd c2) of
    EQ       -> upperCmp s1 s2
    ordering -> ordering

upperSort :: [String] -> [String]
upperSort = sortBy upperCmp

upperValue :: String -> Int
upperValue = foldl (\sum c -> sum + upperOrd c) 0
