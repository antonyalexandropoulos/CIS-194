{-# OPTIONS_GHC -Wall #-}
module Golf where


skips :: [a] -> [[a]]
skips lst = [each i lst | i <- [1..length lst]]


each :: Int  -> [a] -> [a]
each n lst = [lst !! i | i <- [n-1, n-1+n.. length lst-1]]


localMaxima :: [Integer] -> [Integer]
localMaxima (x:rest@(y:z:_) )
  | x < y && y > z = y : localMaxima rest
  | otherwise      = localMaxima rest
localMaxima _      = []

histogram :: [Integer] ->String
histogram xs = unlines (map (line c) [m+1,m..1]) ++  "==========\n0123456789\n"
  where c = count xs
        m = maximum c

line  :: [Int] -> Int -> String
line xs n = [if i>=n then '*' else ' ' | i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]