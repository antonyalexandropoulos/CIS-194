{-# OPTIONS_GHC -Wall #-}
module Whole where
import Data.List

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter even




fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs



fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum 
  . filter even 
  . takeWhile(/=1) 
  . iterate(\x -> if even x then x `div` 2 else 3*x+1)


map' :: (a -> b) -> [a] -> [b]
map' f = foldr(\x y -> f x : y) []

mFoldl :: (a -> b -> a) -> a -> [b] -> a
mFoldl f x = foldr (flip f) x . reverse


--------------------------------------------------------
sieve :: Integer -> [Integer]
sieve n = map ( (+1) . (*2) ) $ [1..n] \\ kappa
  where kappa = map (\(i,j) -> i + j + 2 * i * j )
                . filter (\(i,j) -> i + j + 2 * i * j <=n)
                $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [ (x,y) | x <- xs , y <- ys ]

test :: Integer -> [Integer]
test n =  filter (\x -> if x `mod` 2 == 0 then True else False)[1..n] 

-------------------------------------------------------------

data Tree  a = Leaf 
            | Node Integer (Tree a) a (Tree a)
    deriving (Show,Eq)


foldTree :: Eq  a => [a] -> Tree a
foldTree xs = foldr (balancedInsert start) Leaf xs
 where start = floor (logBase 2 $ fromIntegral(length xs)::Double)

balancedInsert :: Int -> a -> Tree a -> Tree a
balancedInsert _ _ _ = Leaf
 