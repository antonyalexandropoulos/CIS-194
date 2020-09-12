doubleMe x = x + x

sumT  :: [Integer] -> Integer
sumT [] = 0
sumT (x:[]) = x
sumT (x:xs) = x + sumT(xs)

digit :: Integer -> Integer
digit n = n `mod` 10


toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n 
        | n>0 = toDigits (n `div` 10) ++ [digit n]
        | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n 
        | n>0 = (digit n): toDigitsRev (n `div` 10)
        | otherwise = []


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []	= []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs)
        |  (length (x:y:xs)) `mod` 2 /= 0 = x : y*2 : doubleEveryOther xs
        |  otherwise             = x*2 : y :doubleEveryOther xs


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sumT (toDigits(x))
sumDigits (x:xs) = sumT (toDigits(x)) + (sumDigits xs)

validate  :: Integer -> Bool
validate 0 = False
validate n 
		| (sumDigits(doubleEveryOther ( toDigits n ))) `mod` 10 == 0 = True
		|	otherwise 												 = False

type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end temp
        | n <= 0 = []
        | n == 1 = [(start,end)]
        | otherwise = hanoi (n-1) start temp end ++ hanoi 1 start end temp ++ hanoi (n-1) temp end start


