-- Author: Lee Ehudin
-- Main test file for hasked.  The tests are probably a bit sparse since I can't
-- figure out any way to test the curses UI, since the mechanism to send keys
-- to UI.HSCurses.Curses.getCh seems to be internal to the hscurses package.

module Main (main) where

import TestModel (modelTests)
import TestZipper (zipperTests)

import Test.Hspec

-- Run tests for the hasked program
main :: IO ()
main = hspec $ do
    zipperTests
    modelTests
