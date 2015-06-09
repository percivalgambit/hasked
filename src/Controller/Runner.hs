module Controller.Runner (hasked) where

import Model.Buffer (newBuffer, newBufferFromFile, getScreen, getCursorPos)
import View.Curses (updateText, getScreenSize, withCurses)
import Controller.EventHandler (handleEvents)

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

hasked :: IO ()
hasked = withCurses $ do
    args <- getArgs
    buffer <- maybe newBuffer newBufferFromFile (listToMaybe args)
    screenSize <- getScreenSize
    cursorPos <- getCursorPos buffer
    getScreen screenSize buffer >>= updateText cursorPos
    handleEvents screenSize buffer
