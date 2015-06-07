module View.Curses (updateText, getScreenSize, withCurses) where

import Control.Exception (bracket_)
import UI.HSCurses.Curses (stdScr, scrSize, wAddStr, refresh, wclear)
import UI.HSCurses.CursesHelper (drawCursor, gotoTop, start, end)

updateText :: (Int, Int) -> String -> IO ()
updateText cursorPos str = do
    gotoTop
    wclear stdScr
    wAddStr stdScr str
    drawCursor (0,0) cursorPos
    refresh

getScreenSize :: IO (Int, Int)
getScreenSize = scrSize

withCurses :: IO a -> IO a
withCurses = bracket_ start end
