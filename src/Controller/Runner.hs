-- Author: Lee Ehudin
-- Contains the entry point for the text editor

module Controller.Runner (hasked) where

import Controller.EventHandler (handleEvents)
import Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile, getScreen,
                     getCursorPos)
import View.Curses (updateText, getScreenSize, withCurses)

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

-- Entry point for the hasked program.  Sets up an initial buffer by possibly
-- reading from a file, refreshes the screen once, then goes into the event
-- handler loop.
hasked :: IO ()
hasked = withCurses $ do
    args <- getArgs
    buffer <- maybe newBuffer newBufferFromFile (listToMaybe args)
    screenSize <- getScreenSize
    cursorPos <- getCursorPos buffer
    getScreen screenSize buffer >>= updateText cursorPos screenSize
    handleEvents screenSize buffer
    writeBufferToFile buffer
