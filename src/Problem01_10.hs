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

-- Problem 03
elementAt :: (Num k, Eq k) => [a] -> k -> a
elementAt [] k = error "Too few elements in list"
elementAt (x:xs) k = 
    if k==1 
        then x 
        else elementAt xs (k-1)

-- Problem 04
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- Problem 07
data NestedList a = Elem a | List [NestedList a] | Empty deriving (Show, Eq)

myJoin :: [[a]] -> [a]
myJoin [] = []
myJoin [xs] = xs
--myJoin [xs1,xs2] = xs1 ++ xs2
myJoin (xs:xss) = xs ++ (myJoin xss)

myFlatten :: NestedList a -> [a]
myFlatten (Empty) = []
myFlatten (Elem x) = [x]
myFlatten (List (xs)) = myJoin (map myFlatten xs)