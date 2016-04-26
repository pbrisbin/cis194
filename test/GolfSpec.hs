module GolfSpec
    ( main
    , spec
    ) where

import Golf
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Golf" $ do
    describe "skip" $ do
        it "works" $ do
            skips ("ABCD" :: String) `shouldBe` ["ABCD", "BD", "C", "D"]
            skips ("hello!" :: String) `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
            skips [1] `shouldBe` ([[1]] :: [[Int]])
            skips [True,False] `shouldBe` [[True,False], [False]]
            skips [] `shouldBe` ([] :: [[Int]])

    describe "localMaxima" $ do
        it "works" $ do
            localMaxima [2,9,5,6,1] `shouldBe` [9,6]
            localMaxima [2,3,4,1,5] `shouldBe` [4]
            localMaxima [1,2,3,4,5] `shouldBe` []

    describe "histogram" $ do
        it "works" $ do
            histogram [1,1,1,5] `shouldBe` unlines
                [ " *        "
                , " *        "
                , " *   *    "
                , "=========="
                , "0123456789"
                ]

            histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` unlines
                [ "    *     "
                , "    *     "
                , "    * *   "
                , " ******  *"
                , "=========="
                , "0123456789"
                ]
