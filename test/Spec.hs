import Test.Hspec

import Problem01_10

callAll :: [(d -> a)] -> d -> [a]
callAll fns x = [fn x | fn <- fns]


main :: IO ()
main = hspec $ do
    describe "01 myLast" $ do
        it "returns the last element of a list" $ do
            myLast [] `shouldThrow` anyErrorCall
            myLast [1..4] `shouldBe` 4
            myLast ['x','y','z'] `shouldBe` 'z'
            myLast "abc" `shouldBe` 'c'
            -- myLast [] `shouldThrow` anyErrorCall
            -- myLast [1..4] `shouldBe` 4
            -- myLast ['x','y','z'] `shouldBe` 'z'
            -- --fn "abc" `shouldBe` 'c'
            -- map (shouldBe 'c') (fn "abc")
            -- where fn = callAll [myLast]

    describe "02 myButLast" $ do
        it "returns the last but one element of a list" $ do
            myButLast [] `shouldThrow` anyErrorCall
            --myButLast [1] `shouldThrow` anyErrorCall
            myButLast [1..4] `shouldBe` 3
            myButLast ['x','y','z'] `shouldBe` 'y'
            myButLast "abc" `shouldBe` 'b'

    describe "03 elementAt" $ do
        it "returns k-th element of a list" $ do
            elementAt [] 0 `shouldThrow` anyErrorCall
            elementAt [] 1 `shouldThrow` anyErrorCall
            elementAt [1] 1 `shouldBe` 1
            elementAt [1..4] 3 `shouldBe` 3
            elementAt [2..7] 5 `shouldBe` 6
            elementAt "abcdef" 2 `shouldBe` 'b'

    describe "04 myLength" $ do
        it "returns number of elements in a list" $ do
            myLength [] `shouldBe` 0
            myLength [1] `shouldBe` 1
            myLength [5..100] `shouldBe` 96
            myLength "abcdef" `shouldBe` 6

    describe "05 myReverse" $ do
        it "reverses a list" $ do
            myReverse [] `shouldBe` ([] :: [Int])
            myReverse [1] `shouldBe` [1]
            myReverse [3,5,7] `shouldBe` [7,5,3]
            myReverse "abcdef" `shouldBe` "fedcba"

    describe "06 isPalindrome" $ do
        it "checks if a collection is a palindrome" $ do
            isPalindrome ([] :: [Int]) `shouldBe` True
            isPalindrome [1] `shouldBe` True
            isPalindrome [3,5,7] `shouldBe` False
            isPalindrome [3,5,7,5,3] `shouldBe` True
            isPalindrome "ghijkjihg" `shouldBe` True

    describe "07 myFlatten" $ do
        it "transforms a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively)" $ do
            myFlatten (Elem 1) `shouldBe` [1]
            myFlatten (List [Elem 1]) `shouldBe` [1]
            myFlatten (List [Elem 1, Elem 2, List [Elem 4, List [Elem 5]]]) `shouldBe` [1,2,4,5]
            --myFlatten (Empty) `shouldBe` []
            --myFlatten (List [Empty]) `shouldBe` []
