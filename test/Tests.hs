{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Calculator
import Test.Hspec
import Test.Hspec.QuickCheck

constructInput :: (Num a, Show a) => [a] -> [String] -> String
constructInput ns (x : xs) = x ++ concat (zipWith (\n s -> show n ++ s) ns xs)

main :: IO ()
main = hspec $ do
    describe "addition" $ do
        prop "is associative"
            $ \(x :: Integer) (y :: Integer) (z :: Integer) ->
                calculate (constructInput [x, y, z] ["", "+(", "+", ")"])
                    `shouldBe` calculate (constructInput [x, y, z] ["(", "+", ")+", ""])

        prop "is commutative"
            $ \(x :: Integer) (y :: Integer) ->
                calculate (constructInput [x, y] ["", "+", ""])
                    `shouldBe` calculate (constructInput [y, x] ["", "+", ""])

    describe "multiplication" $ do
        prop "is associative"
            $ \(x :: Integer) (y :: Integer) (z :: Integer) ->
                calculate (constructInput [x, y, z] ["", "*(", "*", ")"])
                    `shouldBe` calculate (constructInput [x, y, z] ["(", "*", ")*", ""])

        prop "is commutative"
            $ \(x :: Integer) (y :: Integer) ->
                calculate (constructInput [x, y] ["", "*", ""])
                    `shouldBe` calculate (constructInput [y, x] ["", "*", ""])

        prop "is left distributive"
            $ \(x :: Integer) (y :: Integer) (z :: Integer) ->
                calculate (constructInput [x, y, z] ["", "*(", "+", ")"])
                    `shouldBe` calculate (constructInput [x, y, x, z] ["", "*", "+", "*", ""])

        prop "is right distributive"
            $ \(x :: Integer) (y :: Integer) (z :: Integer) ->
                calculate (constructInput [x, y, z] ["(", "+", ")*", ""])
                    `shouldBe` calculate (constructInput [x, z, y, z] ["", "*", "+", "*", ""])
