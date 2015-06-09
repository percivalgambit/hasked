module Main (main) where

import TestZipper (zipperTests)

import Test.Hspec

main :: IO ()
main = hspec $ do
    zipperTests
