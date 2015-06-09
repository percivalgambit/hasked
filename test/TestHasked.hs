-- Author: Lee Ehudin
-- Main test file for hasked

module Main (main) where

import TestZipper (zipperTests)

import Test.Hspec

-- Run tests for the hasked program
main :: IO ()
main = hspec $ do
    zipperTests
