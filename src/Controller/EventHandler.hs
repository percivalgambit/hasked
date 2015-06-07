module Controller.EventHandler (handleEvents) where

import Model.Buffer (left, right, upLine, downLine, insert, delete, insertNewline,
                     getScreen, getCursorPos, MBuffer)
import View.Curses (updateText)

import Control.Monad (unless)
import UI.HSCurses.Curses (getCh, Key(..))

handleEvents :: MBuffer -> IO ()
handleEvents buffer = do
    key <- getCh
    unless (key == (KeyChar 'q')) $ do
        case key of
            KeyChar c    -> case c of
                '\DEL' -> delete buffer
                '\r'   -> insertNewline buffer
                '\n'   -> insertNewline buffer
                _      -> insert c buffer
            KeyBackspace -> delete buffer
            KeyDown      -> downLine buffer
            KeyUp        -> upLine buffer
            KeyLeft      -> left buffer
            KeyRight     -> right buffer
            KeyEnter     -> insertNewline buffer
            _            -> return ()
        cursorPos <- getCursorPos buffer
        getScreen buffer >>= updateText cursorPos
        handleEvents buffer
