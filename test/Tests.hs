{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Calculator
import Control.Monad (replicateM)
import Data.Foldable (forM_)
import Parser.AST
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- instance Arbitrary Negate where
--     arbitrary = do
--         n <- choose ()

showExpr (Add e m) = showExpr e ++ "+" ++ showMult m
showExpr (Subtract e m) = showExpr e ++ "-" ++ showMult m
showExpr (OfMult m) = showMult m
showMult (Multiply m n) = showMult m ++ "*" ++ showNeg n
showMult (Divide m n) = showMult m ++ "/" ++ showNeg n
showMult (OfNegate n) = showNeg n
showNeg (Neg n) = "-" ++ showNeg n
showNeg (OfAtom a) = showAtom a
showAtom (Number n) = show $ fromRational n
showAtom (Parens e) = "(" ++ showExpr e ++ ")"

maxDepth :: Int
maxDepth = 5

-- Instance for generating random Expr values
instance Arbitrary Expr where
    arbitrary = genExpr 0

genExpr :: Int -> Gen Expr
genExpr depth
    | depth >= maxDepth = OfMult . OfNegate . OfAtom . Number <$> arbitrary
    | otherwise =
        oneof
            [ Add <$> genExpr (depth + 1) <*> genMult (depth + 1)
            , Subtract <$> genExpr (depth + 1) <*> genMult (depth + 1)
            , OfMult <$> genMult (depth + 1)
            ]

-- Instance for generating random Mult values
instance Arbitrary Mult where
    arbitrary = genMult 0

genMult :: Int -> Gen Mult
genMult depth
    | depth >= maxDepth = OfNegate . OfAtom . Number <$> arbitrary
    | otherwise =
        oneof
            [ Multiply <$> genMult (depth + 1) <*> genNegate (depth + 1)
            , Divide <$> genMult (depth + 1) <*> genNegate (depth + 1)
            , OfNegate <$> genNegate (depth + 1)
            ]

-- Instance for generating random Negate values
instance Arbitrary Negate where
    arbitrary = genNegate 0

genNegate :: Int -> Gen Negate
genNegate depth
    | depth >= maxDepth = OfAtom . Number <$> arbitrary
    | otherwise =
        oneof
            [ Neg <$> genNegate (depth + 1)
            , OfAtom <$> genAtom (depth + 1)
            ]

-- Instance for generating random Atom values
instance Arbitrary Atom where
    arbitrary = genAtom 0

genAtom :: Int -> Gen Atom
genAtom depth
    | depth >= maxDepth = Number <$> arbitrary
    | otherwise =
        oneof
            [ Parens <$> genExpr (depth + 1)
            , Number <$> arbitrary
            ]

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
