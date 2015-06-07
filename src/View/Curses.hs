module View.Curses (updateText, getScreenSize, withCurses) where

import Control.Exception (bracket_)
import UI.HSCurses.Curses (stdScr, getYX, scrSize, wAddStr, refresh, wclear)
import UI.HSCurses.CursesHelper (drawCursor, gotoTop, start, end)

updateText :: String -> IO ()
updateText str = do
    (y, x) <- getYX stdScr
    gotoTop
    wclear stdScr
    wAddStr stdScr str
    drawCursor (0,0) (y,x)
    refresh

getScreenSize :: IO (Int, Int)
getScreenSize = scrSize

withCurses :: IO a -> IO a
withCurses = bracket_ start end
