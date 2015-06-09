-- Author: Lee Ehudin
-- Contains all event handling and UI updating code

module Controller.EventHandler (handleEvents) where

import Model.Buffer (MBuffer, ModifyMBuffer, left, right, upLine, downLine,
                     insert, delete, insertNewline, getScreen, getCursorPos,
                     writeBufferToFile)
import View.Curses (updateText)

import Control.Monad (unless)
import UI.HSCurses.Curses (Key(..), getCh)

-- Character codes for actions on certain computers
backspace, enter, escape, save, close :: Char
backspace = '\DEL'
enter     = '\r'
escape    = '\ESC'
save      = '\DC3' -- ctrl + s
close     = '\EOT' -- ctrl + d

-- Maps keys to actions that modify the buffer
handleKey :: Key -> ModifyMBuffer
handleKey (KeyChar c)
    | c == backspace   = delete
    | c == enter       = insertNewline
    | c == save        = writeBufferToFile
    | otherwise        = insert c
handleKey KeyDown      = downLine
handleKey KeyUp        = upLine
handleKey KeyLeft      = left
handleKey KeyRight     = right
handleKey KeyBackspace = delete
handleKey _            = const $ return ()

-- Loop to get a key as input, then modify the buffer and refresh the screen
-- until either the escape key is pressed or ctrl + d
handleEvents :: (Int, Int) -> MBuffer -> IO ()
handleEvents screenSize buffer = do
    key <- getCh
    unless (key == KeyChar escape || key == KeyChar close) $ do
        handleKey key buffer
        cursorPos <- getCursorPos buffer
        getScreen screenSize buffer >>= updateText cursorPos
        handleEvents screenSize buffer
