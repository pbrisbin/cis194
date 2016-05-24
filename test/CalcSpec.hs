module CalcSpec
    ( main
    , spec
    ) where

import Calc
import ExprT

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "eval" $ do
        it "works" $ do
            eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

    describe "evalStr" $ do
        it "parses and evaluates valid expressions" $ do
            evalStr "(2 + 3) * 4" `shouldBe` Just 20

        it "rejects invalid expressions" $ do
            evalStr "whatever * 5" `shouldBe` Nothing

    describe "flexibility" $ do
        it "works as Integer" $ do
            testExp `shouldBe` Just (-7 :: Integer)

        it "works as Bool" $ do
            testExp `shouldBe` Just True

        it "works as MinMax" $ do
            testExp `shouldBe` Just (MinMax 5)

        it "works as Mod7" $ do
            testExp `shouldBe` Just (Mod7 0)

    describe "variables" $ do
        it "works" $ do
            withVars [("x", 6)] (add (lit 3) (var "x")) `shouldBe` Just 9
            withVars [("x", 6)] (add (lit 3) (var "y")) `shouldBe` Nothing
            withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54

  where
    testExp :: Expr a => Maybe a
    testExp = compute "(3 * -4) + 5"
