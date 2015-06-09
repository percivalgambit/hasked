-- Author: Lee Ehudin
-- Defines functions that manipulate the curses UI

module View.Curses (updateText, getScreenSize, withCurses) where

import Control.Exception (bracket_)
import UI.HSCurses.Curses (stdScr, scrSize, wAddStr, refresh, wclear)
import UI.HSCurses.CursesHelper (drawCursor, gotoTop, start, end)

-- Takes in a new cursor position and text to fill the screen with and refreshes
-- the screen with that text and moves the cursor
updateText :: (Int, Int) -> String -> IO ()
updateText cursorPos str = do
    gotoTop
    wclear stdScr
    wAddStr stdScr str
    drawCursor (0,0) cursorPos
    refresh

-- Wrapper around scrSize so we don't need to import Ncurses in the Runner
getScreenSize :: IO (Int, Int)
getScreenSize = scrSize

-- Wrapper around curses actions so curses can be properly initialized and torn
-- down
withCurses :: IO a -> IO a
withCurses = bracket_ start end
