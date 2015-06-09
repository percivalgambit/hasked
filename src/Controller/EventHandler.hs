module Controller.EventHandler (handleEvents) where

import Model.Buffer (left, right, upLine, downLine, insert, delete, insertNewline,
                     getScreen, getCursorPos, writeBufferToFile, MBuffer,
                     ModifyMBuffer)
import View.Curses (updateText)

import Control.Monad (unless)
import UI.HSCurses.Curses (getCh, Key(..))

backspace, enter, escape, save, close :: Char
backspace = '\DEL'
enter     = '\r'
escape    = '\ESC'
save      = '\DC3' -- ctrl + s
close     = '\EOT' -- ctrl + d

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

handleEvents :: (Int, Int) -> MBuffer -> IO ()
handleEvents screenSize buffer = do
    key <- getCh
    unless (key == KeyChar escape || key == KeyChar close) $ do
        handleKey key buffer
        cursorPos <- getCursorPos buffer
        getScreen screenSize buffer >>= updateText cursorPos
        handleEvents screenSize buffer
