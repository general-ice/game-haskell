module Test where

import Data.Char

doubleFact :: Integer -> Integer
doubleFact n
            | n <= 0 = error "Less than nill"
            | (mod n 2) == 0 = product [2,4..n]
            | otherwise = product [1,3..n]



fibonacci'recursive n
            | n == 0 = 0
            | abs n == 1 = 1
            | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci n
            | abs n == 1 = 1
            | n == 0 = zero
            | n >= 0 = pFib first zero first n
            | n <= 0 = nFib first zero (-1) n
            where
                (zero, first) = (0,1)

-- c: current number, its last fibonacci number in the sequence
-- p: prev number, its second from end fibonacci number in the sequence
-- i: i, its number of the current number of fibonacii row
-- n: n number, its just input data, which we transfer in next execute fn

-- for positive numbers
pFib c p i n
            | i < n = pFib (c + p) c  (i + 1) n
            | otherwise = c

-- for negative numbers
nFib c p i n
            | i > n = nFib (p - c) c (i - 1) n
            | otherwise = c