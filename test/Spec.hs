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
            fn "abc" `shouldBe` 'c'
            where fn = callAll [myLast]

