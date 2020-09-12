
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
 deriving (Eq, Show)



(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Single _ a)
  | index == 0 = Just a
  | otherwise = Nothing
indexJ index (Append m l1 l2)
  | index < 0 || index > size0 = Nothing
  | index < size1              = indexJ index l1
  | otherwise                  = indexJ (index - size1) l2
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1
indexJ _ _ = Nothing

getSizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
getSizeJ = getSize . size . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 1 (Single _ _) = Empty
dropJ i jl
  | i<=0 = jl
drop i (Append _ x y)
  | i <  s = dropJ i x +++ y
  | i >= s = dropJ (i-s) y
  where s = getSizeJ x

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _ | i<=0 = Empty
takeJ _ Empty = Empty
takeJ 1 x@(Single _ _ ) = x
takeJ i (Append _ x y) 
  | i < s = takeJ i x
  | i >= s = x +++ takeJ (i-s) y
  where s = getSizeJ x



scoreLine :: String -> JoinList Score String
scoreLine n = Single (scoreString n) n

split :: [a] -> ([a], [a])
split lst = splitAt (((length lst) + 1) `div` 2) lst

instance Buffer (JoinList (Score, Size) String) where
    toString (Empty) = ""
    toString (Single _ a) = a
    toString (Append _ x y) = toString x ++ toString y

    fromString = f .lines
        where f []  = Empty
              f [s] = Single (scoreString s, Size 1) s
              f ss  = f xs +++ f ys
                  where (xs, ys) = split ss 

    line            = indexJ
    replaceLine n l b = takeJ n b +++ (Single (scoreString l,Size 1) l) +++ dropJ (n+1) b
    numLines        = getSizeJ
    value b         = score where (Score score, _) = tag b


jl :: JoinList (Score, Size) String
jl = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor jl