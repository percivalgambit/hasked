module Controller.Runner (hasked) where

import Model.Buffer (newBuffer, getScreen, getCursorPos)
import View.Curses (updateText, getScreenSize, withCurses)
import Controller.EventHandler (handleEvents)

hasked :: IO ()
hasked = withCurses $ do
    buffer <- newBuffer
    screenSize <- getScreenSize
    cursorPos <- getCursorPos buffer
    getScreen screenSize buffer >>= updateText cursorPos
    handleEvents screenSize buffer
