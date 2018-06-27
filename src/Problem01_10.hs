module Problem01_10
where

-- Problem 01
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' = head . reverse

-- Problem 02
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "List has only one element"
myButLast [x1,x2] = x1
myButLast (x:xs) = myButLast xs

-- Problem 02
elementAt :: (Num k, Eq k) => [a] -> k -> a
elementAt [] k = error "Too few elements in list"
elementAt (x:xs) k = 
    if k==1 
        then x 
        else elementAt xs (k-1)