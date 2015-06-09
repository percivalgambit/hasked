{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete, insertNewline,
                     getScreen, getCursorPos, Line, FocusedLine,
                     Lines, Buffer, MBuffer, ModifyMBuffer, ModifyBuffer) where

import qualified Data.ListLike.Zipper as Z

import Control.Exception (catch)
import Control.Lens.At (ix)
import Control.Lens.Getter (to)
import Control.Lens.Operators ((^.), (&), (<&>), (.~), (%~), (^?!))
import Control.Lens.TH (makeLenses)
import Data.Functor ((<$>)) -- needed for base <4.8
import Data.IORef (newIORef, readIORef, modifyIORef', IORef)
import Data.ListLike.Instances ()
import Data.Sequence (Seq)
import qualified Data.ListLike.Base as LL
import qualified Data.ListLike.IO as LLIO
import qualified Data.ListLike.String as LLS
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)

type Line          = Seq Char         -- Single line
type FocusedLine   = Z.Zipper Line    -- Wrapping a Line in a Zipper will allow
                                      -- more efficient edits at the cursor
type Lines         = Seq Line         -- Collection of lines

data Buffer        = Buffer
    { _textLines   :: Z.Zipper Lines  -- All the lines stored in the buffer
    , _focusedLine :: FocusedLine     -- Cache for the line we are currently editing
    , _saveFile    :: Maybe FilePath  -- Filepath this buffer is associated with
    } deriving Show
makeLenses ''Buffer
type MBuffer       = IORef Buffer     -- Mutable buffer
type ModifyMBuffer = MBuffer -> IO () -- Action to modify a mutable buffer
type ModifyBuffer  = Buffer -> Buffer -- Action to 'modify' an immutable buffer

-- Load the current focus of textLines into focusedLine, and move the cursor to
-- the specified position on the line.  If there is no focus, load an empty line
focusLine :: Int -> ModifyBuffer
focusLine linePos buf = buf & focusedLine .~
    buf^.textLines.to Z.safeCursor.to (\focus -> case focus of
        Nothing    -> Z.empty
        Just line  -> iterate Z.right (Z.zip line) ^?! ix linePos)

-- Get the current line position from focusedLine, then load the current focus of
-- textLines into focusedLine with the same line position.  This is useful when
-- moving up and down the lines to make sure that the cursor retains approximately
-- the same line position between any two lines.  If the new focusedLine is
-- shorter than the old one, the cursor will just be at the end of the new line.
refocusLine :: ModifyBuffer
refocusLine = getLinePos >>= focusLine -- wow, an actual use of the (->) monad

-- Load the current focus of textLines into focusedLine and move the cursor to
-- the beginning of the line
focusLineBeginning :: ModifyBuffer
focusLineBeginning = focusLine 0

-- Load the current focus of textLines into focusedLine and move the cursor to
-- the end of the line
focusLineEnd :: ModifyBuffer
focusLineEnd buffer = refocusLine buffer & focusedLine %~ Z.end

-- Ensure that the line cached in focusLine has been written to the current focus
-- of textLines
flushFocusedLine :: ModifyBuffer
flushFocusedLine buf = buf & textLines %~ \lines' ->
    if | Z.endp lines' && not (LL.null focus) -> Z.insert focus lines'
       | otherwise                            -> Z.replace focus lines' where
    focus = buf^.focusedLine & Z.unzip

-- Create a new mutable buffer
newBuffer :: IO MBuffer
newBuffer = newIORef $ Buffer Z.empty Z.empty Nothing

-- Create a new buffer and associate it with a filepath.  If the file already
-- exists and there is no error in opening it, the contents will be loaded into
-- the buffer.  If the file does not exist, it will be created, and an empty
-- buffer associated with that filepath will be returned.
newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = do
    fileLines <- Z.zip . LLS.lines <$> LLIO.readFile filepath `catch` handler
    newIORef $ refocusLine $ Buffer fileLines Z.empty (Just filepath) where
        handler e
            | isDoesNotExistError e = return LL.empty
            | isAlreadyInUseError e =
                error $ "File " ++ filepath ++ " is already in use: " ++ show e
            | isPermissionError   e =
                error $ "You do not have permission to open " ++ filepath ++ ": "
                        ++ show e
            | otherwise             =
                error $ "There was an error when opening " ++ filepath ++ ": "
                        ++ show e

-- If buffer is associated with a filepath, the current contents of the file
-- located at that filepath will be overwritten by the current contents of the
-- buffer.  If the buffer is not associated with a filepath, then nothing will
-- happen.
writeBufferToFile :: ModifyMBuffer
writeBufferToFile buffer = do
    modifyIORef' buffer flushFocusedLine
    frozenBuffer <- readIORef buffer
    case frozenBuffer^.saveFile of
        Just filepath -> frozenBuffer^.textLines & Z.unzip
                                                 & LLS.unlines
                                                 & LLIO.writeFile filepath
        Nothing       -> return ()

-- Given a (y, x) pair that represents the screen size, at most a single screen
-- worth of the buffer will be returned in the form of a string, meant to be
-- displayed in the terminal.
getScreen :: (Int, Int) -> MBuffer -> IO String
getScreen (y, x) buffer = do
    modifyIORef' buffer flushFocusedLine
    readIORef buffer <&> \frozenBuffer ->
        frozenBuffer^.textLines & Z.unzip
                                & LL.take y
                                & fmap (LL.take x)
                                & LLS.unlines
                                & LLS.toString

-- Using the two Zippers representing the current line in the file (textLines)
-- and the current position in the line (focusedLine), return the (y, x) coordinates
-- of the cursor (focuses)
getCursorPos :: MBuffer -> IO (Int, Int)
getCursorPos buffer = readIORef buffer <&> \frozenBuffer -> do
    let y = frozenBuffer^.textLines & Z.position
    let x = getLinePos frozenBuffer
    (y, x)

-- Helper function to return the position of the focus of focusedLine, without
-- it being trapped in a tuple and in the IO monad
getLinePos :: Buffer -> Int
getLinePos buffer = buffer^.focusedLine & Z.position

-- Move the cursor one position to the left.  If the cursor is at the beginning
-- of a line, it will move up to the end of the previous line, if there is one.
left :: ModifyMBuffer
left buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let focus = frozenBuffer^.focusedLine
    let lines' = frozenBuffer^.textLines
    if | Z.beginp focus && not (Z.beginp lines') ->
        frozenBuffer & textLines %~ Z.left & focusLineEnd
       | otherwise      -> frozenBuffer & focusedLine %~ Z.left

-- Move the cursor one position to the right.  If the cursor is at the end of a
-- line, it will move up to the beginning of the next line, if there is one.
right :: ModifyMBuffer
right buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let focus  = frozenBuffer^.focusedLine
    let lines' = frozenBuffer^.textLines
    if | Z.endp focus && not (Z.endp lines') ->
        frozenBuffer & textLines %~ Z.right & focusLineBeginning
       | otherwise    -> frozenBuffer & focusedLine %~ Z.right

-- Move the cursor one line up, if possible
upLine :: ModifyMBuffer
upLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.left & refocusLine

-- Move the cursor one line down, if possible
downLine :: ModifyMBuffer
downLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.right & refocusLine

-- Insert a character into the line cached in focusedLine, at the position of
-- the cursor
insert :: Char -> ModifyMBuffer
insert c buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusedLine %~ Z.push c

-- Insert a newline into the buffer at the position of the cursor.  If there
-- is text to the right of the cursor, it will be moved down to the inserted line,
-- otherwise the inserted line will be blank.  The cursor will move to the
-- beginning of the new line.
insertNewline :: ModifyMBuffer
insertNewline buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let lineRest = frozenBuffer^.focusedLine & Z.getRight
    frozenBuffer & focusedLine %~ Z.dropRight
                 & flushFocusedLine
                 & textLines %~ (\lines' ->
                    if | Z.endp lines' -> Z.push LL.empty lines'
                       | otherwise     -> Z.right lines' & Z.insert lineRest)
                 & focusLineBeginning

-- Delete the character to the left of the cursor, if there is one.  If the cursor
-- is at the beginning of a line, it will delete the newline, causing the contents
-- of that line to move up and join with the contents of the previous line.  The
-- cursor will then be located at the old end of the upper line.
delete :: ModifyMBuffer
delete buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let focus = frozenBuffer^.focusedLine
    let lines' = frozenBuffer^.textLines
    if | Z.beginp focus && not (Z.beginp lines') ->
        frozenBuffer & flushFocusedLine
                     & textLines %~ deleteNewline
                     & focusLineEnd
                     & textLines %~ cursorAppend (Z.unzip focus)
                     & refocusLine
       | otherwise -> frozenBuffer & focusedLine %~ Z.pop
    where
        deleteNewline lines' = case Z.safeCursor lines' of
            Just _  -> Z.delete lines' & Z.left
            Nothing -> Z.left lines'
        cursorAppend line lines' = case Z.safeCursor lines' of
            Just line' -> Z.replace (line' `LL.append` line) lines'
            Nothing -> Z.insert line lines'
