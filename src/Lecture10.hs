--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 10: Lazy evaluation                                                --
--------------------------------------------------------------------------------

module Lecture10 where

import Prelude hiding (map, take, length)

--------------------------------------------------------------------------------
-- fac' with explicit case expression

fac' :: Int -> Int -> Int
fac' n m = case n of
    0 -> m
    _ -> fac' (n-1) (n*m)

--------------------------------------------------------------------------------
-- fac' with explicit let-bound arguments

fac'' :: Int -> Int -> Int
fac'' n m = case n of
    0 -> m
    _ -> let x = n-1
             y = n*m
         in fac'' x y

--------------------------------------------------------------------------------
-- Strict application

fac''' :: Int -> Int -> Int
fac''' 0 m = m
fac''' n m = (fac''' $! (n-1)) (n*m)

--------------------------------------------------------------------------------
-- Internal representations of map, take, and length generated and
-- used by the compiler

map :: (a -> b) -> [a] -> [b]
map = \f -> \arg -> case arg of
    []     -> []
    (x:xs) -> let y  = f x
                  ys = map f xs
              in y : ys

take :: Int -> [a] -> [a]
take = \n -> \xs -> case n of
    0  -> []
    n' -> case xs of
        []     -> []
        (y:ys) -> let x  = n' - 1
                      zs = take x ys
                  in y : zs

length :: [a] -> Int
length = \xs -> case xs of
    []     -> 0
    (y:ys) -> let n = length ys
              in 1 + n

--------------------------------------------------------------------------------
-- Infinite data structures

from :: Int -> [Int]
from n = n : from (n+1)

e0 :: [Int]
e0 = take 3 (from 4)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

--------------------------------------------------------------------------------
