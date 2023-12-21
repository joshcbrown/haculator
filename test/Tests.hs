{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Calculator
import Calculator.Parser (Expr (..), showExpr)
import Control.Monad (replicateM)
import Data.Foldable (forM_)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary Expr where
    arbitrary = do
        n <- choose (0 :: Int, 2)
        case n of
            0 -> Number <$> arbitrary
            1 -> Negate <$> arbitrary
            2 -> BinOp <$> arbitraryBoundedEnum <*> arbitrary <*> arbitrary

constructInput :: [Expr] -> [String] -> String
constructInput ns (x : xs) = x ++ concat (zipWith (\n s -> showExpr n ++ s) ns xs)

getRandomInput :: IO ()
getRandomInput = do
    ns <- sample' (arbitrary :: Gen Expr)
    forM_ ns $ \expr -> putStr (showExpr expr ++ "\n\n\n")

main :: IO ()
main = hspec $ do
    describe "addition" $ do
        prop "is associative"
            $ \(x :: Expr) (y :: Expr) (z :: Expr) ->
                calculate (constructInput [x, y, z] ["", "+(", "+", ")"])
                    `shouldBe` calculate (constructInput [x, y, z] ["(", "+", ")+", ""])

        prop "is commutative"
            $ \(x :: Expr) (y :: Expr) ->
                calculate (constructInput [x, y] ["", "+", ""])
                    `shouldBe` calculate (constructInput [y, x] ["", "+", ""])

    describe "multiplication" $ do
        prop "is associative"
            $ \(x :: Expr) (y :: Expr) (z :: Expr) ->
                calculate (constructInput [x, y, z] ["", "*(", "*", ")"])
                    `shouldBe` calculate (constructInput [x, y, z] ["(", "*", ")*", ""])

        prop "is commutative"
            $ \(x :: Expr) (y :: Expr) ->
                calculate (constructInput [x, y] ["", "*", ""])
                    `shouldBe` calculate (constructInput [y, x] ["", "*", ""])

        prop "is left distributive"
            $ \(x :: Expr) (y :: Expr) (z :: Expr) ->
                calculate (constructInput [x, y, z] ["", "*(", "+", ")"])
                    `shouldBe` calculate (constructInput [x, y, x, z] ["", "*", "+", "*", ""])

        prop "is right distributive"
            $ \(x :: Expr) (y :: Expr) (z :: Expr) ->
                calculate (constructInput [x, y, z] ["(", "+", ")*", ""])
                    `shouldBe` calculate (constructInput [x, z, y, z] ["", "*", "+", "*", ""])
