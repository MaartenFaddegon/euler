module Libmatrix where
import Liblist
import Libnum

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

sumRow prevSums row = sh : (sm ++ [sl])
    where sh = head prevSums + head row
          sl = last prevSums + last row
          sm = zipfold sumWithMax (tail row) prevSums

maximumPathSum triangle = maximum $ foldl sumRow (head triangle) (tail triangle)
