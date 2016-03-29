module HW1Spec
    ( main
    , spec
    ) where

import HW1
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "toDigits(Rev)" $ do
        it "works" $ do
            toDigits 1234 `shouldBe` [1,2,3,4]
            toDigitsRev 1234 `shouldBe` [4,3,2,1]
            toDigits 0 `shouldBe` []
            toDigits (-17) `shouldBe` []

    describe "doubleEveryOther" $ do
        it "works" $ do
            doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
            doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

    describe "sumDigits" $ do
        it "works" $ do
            sumDigits [16,7,12,5] `shouldBe` 22

    describe "validate" $ do
        it "works" $ do
            validate 4012888888881881 `shouldBe` True
            validate 4012888888881882 `shouldBe` False

    describe "hanoi" $ do
        it "works" $ do
            hanoi 2 "a" "b" "c" `shouldBe`
                [ ("a","c")
                , ("a","b")
                , ("c","b")
                ]
