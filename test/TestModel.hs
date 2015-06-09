{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

-- Author: Lee Ehudin
-- Defines tests for the Model

module TestModel (modelTests) where

import Model.Buffer (ModifyMBuffer, newBuffer, newBufferFromFile,
                     writeBufferToFile, left, right, upLine, downLine, insert,
                     delete, insertNewline, getScreen, getCursorPos)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Prelude hiding (Left, Right)
import System.IO (hClose, hPutStrLn, openTempFile)

-- data type representing an arbitrary movement of the cursor
data Movement = Left | Right | Up | Down deriving Show

instance Arbitrary Movement where
    arbitrary = do
        n <- choose (0,3) :: Gen Int
        return $ case n of
            0 -> Left
            1 -> Right
            2 -> Up
            3 -> Down
            _ -> error "Weird randomness happened"

-- Execute a movement of the cursor
move :: Movement -> ModifyMBuffer
move Left  = left
move Right = right
move Up    = upLine
move Down  = downLine

-- Insert a string into a buffer
insertString :: String -> ModifyMBuffer
insertString str b = mapM_ (flip insert b) str

-- Run tests on the Model
modelTests :: Spec
modelTests = do
    describe "Buffer properties" $ do
        prop "can load text and write from any arbitrary file it has permissions\
             \ for"
            $ \s -> do
                (tmpFilepath, tmpHandle) <- openTempFile "/tmp" "hasked_test"
                hPutStrLn tmpHandle s
                hClose tmpHandle
                b <- newBufferFromFile tmpFilepath
                s' <- getScreen (length s, length s) b
                if | null s'   -> s' `shouldBe` s
                   | otherwise -> s' `shouldBe` s ++ "\n"
                writeBufferToFile b
                s'' <- readFile tmpFilepath
                s'' `shouldBe` s ++ "\n"

        specify "an empty buffer should not have any text in it" $ do
            b <- newBuffer
            s <- getScreen (1, 1) b
            s `shouldBe` ""

        prop "the cursor cannot change if the buffer is empty" $ \m -> do
            b <- newBuffer
            move m b
            cursorPos <- getCursorPos b
            cursorPos `shouldBe` (0, 0)

        prop "inserting text into a buffer should leave it intact, no matter\
             \ what sequence of movement happens" $ \s (ms :: [Movement]) -> do
                b <- newBuffer
                insertString s b
                mapM_ (flip move b) ms
                s' <- getScreen (length s, length s) b
                if | null s'   -> s' `shouldBe` s
                   | otherwise -> s' `shouldBe` s ++ "\n"

    describe "Buffer manual tests" $
        specify "the cursor position should update if the cursor moves" $ do
            b <- newBuffer
            insertString "Hello, world" b
            insertNewline b
            insertNewline b
            insertString "foobar" b
            pos1 <- getCursorPos b
            pos1 `shouldBe` (2, 6)
            upLine b
            left b
            pos2 <- getCursorPos b
            pos2 `shouldBe` (0, 12)
            right b
            pos3 <- getCursorPos b
            pos3 `shouldBe` (1, 0)
            delete b
            pos4 <- getCursorPos b
            pos4 `shouldBe` (0, 12)
