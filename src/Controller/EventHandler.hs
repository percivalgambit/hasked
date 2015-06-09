module Controller.EventHandler (handleEvents) where

import Model.Buffer (left, right, upLine, downLine, insert, delete, insertNewline,
                     getScreen, getCursorPos, MBuffer, ModifyBuffer)
import View.Curses (updateText)

import Control.Monad (unless)
import UI.HSCurses.Curses (getCh, Key(..))

backspace, enter, escape :: Char
backspace = '\DEL'
enter     = '\r'
escape    = '\ESC'

handleKey :: Key -> ModifyBuffer
handleKey (KeyChar c)
    | c == backspace   = delete
    | c == enter       = insertNewline
    | otherwise        = insert c
handleKey KeyDown      = downLine
handleKey KeyUp        = upLine
handleKey KeyLeft      = left
handleKey KeyRight     = right
handleKey KeyBackspace = delete
handleKey _            = const $ return ()

handleEvents :: (Int, Int) -> MBuffer -> IO ()
handleEvents screenSize buffer = do
    key <- getCh
    unless (key == KeyChar escape) $ do
        handleKey key buffer
        cursorPos <- getCursorPos buffer
        getScreen screenSize buffer >>= updateText cursorPos
        handleEvents screenSize buffer
