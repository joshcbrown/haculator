{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Calculator
import Control.Monad (replicateM)
import Data.Foldable (forM_)
import Data.Ratio
import Parser.AST
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- instance Arbitrary Negate where
--     arbitrary = do
--         n <- choose ()

exprToString :: Expr -> String
exprToString (Add e1 e2) = "(" ++ exprToString e1 ++ "+" ++ multToString e2 ++ ")"
exprToString (Subtract e1 e2) = "(" ++ exprToString e1 ++ "-" ++ multToString e2 ++ ")"
exprToString (OfMult mult) = multToString mult

multToString :: Mult -> String
multToString (Multiply m1 m2) = "(" ++ multToString m1 ++ "*" ++ negateToString m2 ++ ")"
multToString (Divide m1 m2) = "(" ++ multToString m1 ++ "/" ++ negateToString m2 ++ ")"
multToString (OfNegate negate) = negateToString negate

negateToString :: Negate -> String
negateToString (Neg n) = "-(" ++ negateToString n ++ ")"
negateToString (OfAtom atom) = atomToString atom

atomToString :: Atom -> String
atomToString (Number r) = (show . fromRational) r
atomToString (Parens e) = "(" ++ exprToString e ++ ")"

maxDepth :: Int
maxDepth = 5

instance Arbitrary Expr where
    arbitrary = genExpr 0

genExpr :: Int -> Gen Expr
genExpr depth
    | depth >= maxDepth = OfMult . OfNegate . OfAtom . Number <$> nonZero
    | otherwise =
        oneof
            [ Add <$> genExpr (depth + 1) <*> genMult (depth + 1)
            , Subtract <$> genExpr (depth + 1) <*> genMult (depth + 1)
            , OfMult <$> genMult (depth + 1)
            ]

instance Arbitrary Mult where
    arbitrary = genMult 0

genMult :: Int -> Gen Mult
genMult depth
    | depth >= maxDepth = OfNegate . OfAtom . Number <$> nonZero
    | otherwise =
        oneof
            [ Multiply <$> genMult (depth + 1) <*> genNegate (depth + 1)
            , Divide <$> genMult (depth + 1) <*> genNegate (depth + 1)
            , OfNegate <$> genNegate (depth + 1)
            ]

instance Arbitrary Negate where
    arbitrary = genNegate 0

genNegate :: Int -> Gen Negate
genNegate depth
    | depth >= maxDepth = OfAtom . Number <$> nonZero
    | otherwise =
        oneof
            [ Neg <$> genNegate (depth + 1)
            , OfAtom <$> genAtom (depth + 1)
            ]

nonZero :: Gen Rational
nonZero = suchThat arbitrary (/= 0)

instance Arbitrary Atom where
    arbitrary = genAtom 0

genAtom :: Int -> Gen Atom
genAtom depth
    | depth >= maxDepth = Number <$> nonZero
    | otherwise =
        oneof
            [ Parens <$> genExpr (depth + 1)
            , Number <$> arbitrary
            ]

constructInput :: [Expr] -> [String] -> String
constructInput ns (x : xs) = x ++ concat (zipWith (\n s -> exprToString n ++ s) ns xs)

getRandomInput :: IO ()
getRandomInput = do
    ns <- sample' (arbitrary :: Gen Expr)
    forM_ ns $ \expr -> putStr (exprToString expr ++ "\n\n\n")

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
