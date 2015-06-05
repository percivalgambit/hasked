module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete) where

import qualified Data.ListLike.Zipper as Z

import Data.IORef (newIORef, readIORef, modifyIORef', IORef)
import Data.ListLike.Instances ()
import qualified Data.ListLike.IO as LLIO
import qualified Data.ListLike.String as LLS
import qualified Data.Sequence as S

type Line         = S.Seq Char
type FocusedLine  = Z.Zipper Line
type Lines        = S.Seq Line

data Buffer       = Buffer
    { _textLines   :: Z.Zipper Lines
    , _focusedLine :: FocusedLine
    }
type MBuffer      = IORef Buffer
type ModifyBuffer = MBuffer -> IO ()

focusLine :: ModifyBuffer
focusLine = flip modifyIORef' $ \frozenBuffer -> case () of
    _ | Z.emptyp (_focusedLine frozenBuffer) ->
        frozenBuffer {_focusedLine = Z.zip $ Z.cursor $ _textLines frozenBuffer}
      | otherwise -> frozenBuffer

flushFocusedLine :: ModifyBuffer
flushFocusedLine = flip modifyIORef' $ \frozenBuffer -> case () of
    _ | Z.emptyp (_focusedLine frozenBuffer) -> frozenBuffer
      | otherwise -> frozenBuffer
        { _textLines   = Z.replace (Z.unzip $ _focusedLine frozenBuffer)
                                   (_textLines frozenBuffer)
        , _focusedLine = Z.empty
        }

newBuffer :: IO MBuffer
newBuffer = newIORef $ Buffer Z.empty Z.empty

newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = do
    fileLines <- Z.zip . LLS.lines <$> LLIO.readFile filepath
    let focus = Z.zip $ Z.cursor fileLines
    newIORef $ Buffer fileLines focus

writeBufferToFile :: FilePath -> ModifyBuffer
writeBufferToFile filepath buffer = do
    flushFocusedLine buffer
    bufferLines <- _textLines <$> readIORef buffer
    LLIO.writeFile filepath $ LLS.unlines $ Z.unzip bufferLines

left, right, upLine, downLine :: ModifyBuffer
left buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer {_focusedLine = Z.left $ _focusedLine frozenBuffer}

right buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer {_focusedLine = Z.right $ _focusedLine frozenBuffer}

upLine buffer = do
    flushFocusedLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer {_textLines = Z.left $ _textLines frozenBuffer}

downLine buffer = do
    flushFocusedLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer {_textLines = Z.right $ _textLines frozenBuffer}

insert :: Char -> ModifyBuffer
insert c buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer {_focusedLine = Z.insert c $ _focusedLine frozenBuffer}

delete :: ModifyBuffer
delete buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer {_focusedLine = Z.delete $ _focusedLine frozenBuffer}
