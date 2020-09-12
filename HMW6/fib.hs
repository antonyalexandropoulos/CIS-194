
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where


fib :: Integer -> Integer
fib 0 = 0
fib n 
   | n > 1  = fib (n-1) + fib (n-2)
   | n == 1 = 1


fibs1 :: [Integer]
fibs1  = map fib [0..]

fibs2 :: [Integer]
fibs2  = fibgen 0 1 where
	fibgen a b =  a : fibgen b (a+b)


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
	show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a) 


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a rest) = Cons (f a) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))


nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = startRuler 0 where
	startRuler n = interLeaveStreams (streamRepeat n) (startRuler (succ n))

interLeaveStreams :: Stream a -> Stream a -> Stream a
interLeaveStreams (Cons x xs) (Cons y ys) =
	    Cons x (Cons y ( interLeaveStreams ys xs)) 




x :: Stream Integer
x = Cons 0(Cons 1 (streamRepeat 0))




instance Num (Stream Integer) where
	fromInteger n = Cons n (streamRepeat 0)
	negate (Cons y ys) = Cons (-y) (negate ys)
	(+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs + ys)
	(*) (Cons x xs) s@(Cons y ys) = Cons (x*y) (streamMap (*x) ys + (xs*s))

instance Fractional (Stream Integer) where
    (/) (Cons x xs) (Cons y ys) = q
        where q = Cons (x `div` y) (streamMap (`div` y) (xs - q * ys))

fibs10 :: Stream Integer
fibs10 = x / (1 - x - x * x)

data Matrix  = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
	(*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22)=
		(Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22))
				(a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22 )
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fst' (m^(n-1))
	where m = Matrix 1 1 1 0


fst' :: Matrix -> Integer
fst' (Matrix x _ _ _) = x

lst' :: [a] -> a
lst' [x] = x
lst' (x:xs) = lst' xs

