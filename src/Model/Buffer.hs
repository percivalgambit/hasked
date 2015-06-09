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

type Line          = Seq Char
type FocusedLine   = Z.Zipper Line
type Lines         = Seq Line

data Buffer        = Buffer
    { _textLines   :: Z.Zipper Lines
    , _focusedLine :: FocusedLine
    , _saveFile    :: Maybe FilePath
    } deriving Show
makeLenses ''Buffer
type MBuffer       = IORef Buffer -- mutable buffer
type ModifyMBuffer = MBuffer -> IO ()
type ModifyBuffer  = Buffer -> Buffer

focusLine :: Int -> ModifyBuffer
focusLine linePos buf = buf & focusedLine .~
    buf^.textLines.to Z.safeCursor.to (\focus -> case focus of
        Nothing    -> Z.empty
        Just line  -> iterate Z.right (Z.zip line) ^?! ix linePos)

refocusLine :: ModifyBuffer
refocusLine = getLinePos >>= focusLine

focusLineBeginning :: ModifyBuffer
focusLineBeginning = focusLine 0

focusLineEnd :: ModifyBuffer
focusLineEnd buffer = refocusLine buffer & focusedLine %~ Z.end

flushFocusedLine :: ModifyBuffer
flushFocusedLine buf = buf & textLines %~ \lines' ->
    if | Z.endp lines' && not (LL.null focus) -> Z.insert focus lines'
       | otherwise                            -> Z.replace focus lines' where
    focus = buf^.focusedLine & Z.unzip

newBuffer :: IO MBuffer
newBuffer = newIORef $ Buffer Z.empty Z.empty Nothing

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

writeBufferToFile :: ModifyMBuffer
writeBufferToFile buffer = do
    modifyIORef' buffer flushFocusedLine
    frozenBuffer <- readIORef buffer
    case frozenBuffer^.saveFile of
        Just filepath -> frozenBuffer^.textLines & Z.unzip
                                                 & LLS.unlines
                                                 & LLIO.writeFile filepath
        Nothing       -> return ()

getScreen :: (Int, Int) -> MBuffer -> IO String
getScreen (y, x) buffer = do
    modifyIORef' buffer flushFocusedLine
    readIORef buffer <&> \frozenBuffer ->
        frozenBuffer^.textLines & Z.unzip
                                & LL.take y
                                & fmap (LL.take x)
                                & LLS.unlines
                                & LLS.toString

getCursorPos :: MBuffer -> IO (Int, Int)
getCursorPos buffer = readIORef buffer <&> \frozenBuffer -> do
    let y = frozenBuffer^.textLines & Z.position
    let x = getLinePos frozenBuffer
    (y, x)

getLinePos :: Buffer -> Int
getLinePos buffer = buffer^.focusedLine & Z.position

left, right, upLine, downLine :: ModifyMBuffer
left buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let focus = frozenBuffer^.focusedLine
    let lines' = frozenBuffer^.textLines
    if | Z.beginp focus && not (Z.beginp lines') ->
        frozenBuffer & textLines %~ Z.left & focusLineEnd
       | otherwise      -> frozenBuffer & focusedLine %~ Z.left

right buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let focus  = frozenBuffer^.focusedLine
    let lines' = frozenBuffer^.textLines
    if | Z.endp focus && not (Z.endp lines') ->
        frozenBuffer & textLines %~ Z.right & focusLineBeginning
       | otherwise    -> frozenBuffer & focusedLine %~ Z.right

upLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.left & refocusLine

downLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.right & refocusLine

insert :: Char -> ModifyMBuffer
insert c buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusedLine %~ Z.push c

insertNewline :: ModifyMBuffer
insertNewline buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let lineRest = frozenBuffer^.focusedLine & Z.getRight
    frozenBuffer & focusedLine %~ Z.dropRight
                 & flushFocusedLine
                 & textLines %~ (\lines' ->
                    if | Z.endp lines' -> Z.push LL.empty lines'
                       | otherwise     -> Z.right lines' & Z.insert lineRest)
                 & focusLineBeginning

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
