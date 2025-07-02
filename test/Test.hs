{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveGeneric      #-}

module Main where

import GHC.Generics
import Data.Monoid.Generic

import Test.Tasty
import Test.Tasty.HUnit
import Data.Semigroup

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

data TestData = TestData (Sum Int) (Max Int) (Min Int)
    deriving (Generic, Eq, Show)
    deriving Semigroup via GenericSemigroup TestData
    deriving Monoid    via GenericMonoid    TestData

unitTests :: TestTree
unitTests = testGroup "Unit"
    [ testCase "monoid" $ do
        TestData 0 minBound maxBound @=? mempty
    , testCase "semigroup" $ do
        TestData 10 2 5 @=? TestData 4 0 5 <> TestData 6 2 10
    ]
