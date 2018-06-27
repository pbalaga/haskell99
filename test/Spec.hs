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
