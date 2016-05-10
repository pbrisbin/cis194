module HW4Spec
    ( main
    , spec
    ) where

import HW4

import Test.Hspec
import Test.QuickCheck

fun1Ref :: [Integer] -> Integer
fun1Ref [] = 1
fun1Ref (x:xs)
    | even x = (x - 2) * fun1Ref xs
    | otherwise = fun1Ref xs

-- fun2Ref :: Integer -> Integer
-- fun2Ref 1 = 0
-- fun2Ref n
--     | even n = n + fun2Ref (n `div` 2)
--     | otherwise = fun2Ref (3 * n + 1)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fun1" $ do
        it "works" $ property $ \is ->
            fun1 is `shouldBe` fun1Ref is

    -- describe "fun2" $ do
    --     it "works" $ property $ \is ->
    --         fun2 is `shouldBe` fun2Ref is

    describe "foldTree" $ do
        it "works" $ property $ \as ->
            isBalanced $ foldTree (as :: String)

    describe "xor" $ do
        it "works" $ do
            xor [False, True, False] `shouldBe` True
            xor [False, True, False, False, True] `shouldBe` False

    describe "map" $ do
        it "works" $ property $ \is ->
            map (+1) is `shouldBe` map' (+1) (is :: [Int])

    describe "sieveSundaram" $ do
        it "works" $ do
            sieveSundaram 202 `shouldBe`
                [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53
                , 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113
                , 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181
                , 191, 193, 197, 199
                ]
