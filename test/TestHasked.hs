{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.ListLike.FunctorLL as FLL
import qualified Data.ListLike.Zipper as Z

import TestLaws ()

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "The Zipper data type" $ do
        prop "satisfies the functor laws" $ \(z :: Z.Zipper [Int]) ->
            FLL.fmap id z `shouldBe` id z
