module Controller.Runner (hasked) where

import Model.Buffer (newBuffer, setScreenSize, getScreen, getCursorPos)
import View.Curses (updateText, getScreenSize, withCurses)
import Controller.EventHandler (handleEvents)

hasked :: IO ()
hasked = withCurses $ do
    buffer <- newBuffer
    scrSize <- getScreenSize
    setScreenSize scrSize buffer
    cursorPos <- getCursorPos buffer
    getScreen buffer >>= updateText cursorPos
    handleEvents buffer
