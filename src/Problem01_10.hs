module Problem01_10
    ( myLast
    ) where

-- Problem 01
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' = head . reverse

-- Problem 02