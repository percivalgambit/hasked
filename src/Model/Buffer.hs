{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete, insertNewline,
                     getScreen, getCursorPos, Line, FocusedLine,
                     Lines, Buffer, MBuffer, ModifyBuffer) where

import qualified Data.ListLike.Zipper as Z

import Control.Lens.At (ix)
import Control.Lens.Getter (to)
import Control.Lens.Operators ((^.), (&), (<&>), (.~), (%~), (^?!))
import Control.Lens.TH (makeLenses)
import Data.Functor ((<$>)) -- needed for base <4.8
import Data.IORef (newIORef, readIORef, modifyIORef', IORef)
import Data.ListLike.Instances ()
import qualified Data.ListLike.Base as LL
import qualified Data.ListLike.IO as LLIO
import qualified Data.ListLike.String as LLS
import qualified Data.Sequence as S

type Line         = S.Seq Char
type FocusedLine  = Z.Zipper Line
type Lines        = S.Seq Line

data Buffer       = Buffer
    { _textLines   :: Z.Zipper Lines
    , _focusedLine :: FocusedLine
    } deriving Show
makeLenses ''Buffer
type MBuffer      = IORef Buffer -- mutable buffer
type ModifyBuffer = MBuffer -> IO ()

focusLine :: Int -> Buffer -> Buffer
focusLine linePos buf = buf & focusedLine .~
    buf^.textLines.to Z.safeCursor.to (\focus -> case focus of
        Nothing    -> Z.empty
        Just line  -> iterate Z.right (Z.zip line) ^?! ix linePos)

refocusLine :: Buffer -> Buffer
refocusLine = getLinePos >>= focusLine

flushFocusedLine :: Buffer -> Buffer
flushFocusedLine buf = buf & textLines %~ \lines' ->
    if | Z.endp lines' -> Z.insert focus lines'
       | otherwise     -> Z.replace focus lines' where
    focus = buf^.focusedLine & Z.unzip

newBuffer :: IO MBuffer
newBuffer = newIORef $ Buffer Z.empty Z.empty

newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = do
    fileLines <- Z.zip . LLS.lines <$> LLIO.readFile filepath
    newIORef $ refocusLine $ Buffer fileLines Z.empty

writeBufferToFile :: FilePath -> ModifyBuffer
writeBufferToFile filepath buffer = do
    modifyIORef' buffer flushFocusedLine
    frozenBuffer <- readIORef buffer
    frozenBuffer^.textLines
                 .to Z.unzip
                 .to LLS.unlines
                 .to (LLIO.writeFile filepath)

getScreen :: (Int, Int) -> MBuffer -> IO String
getScreen (y, x) buffer = do
    modifyIORef' buffer flushFocusedLine
    readIORef buffer <&> \frozenBuffer ->
        frozenBuffer^.textLines
                     .to Z.unzip
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

left, right, upLine, downLine :: ModifyBuffer
left buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusedLine %~ Z.left

right buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusedLine %~ Z.right

upLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.left & refocusLine

downLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.right & refocusLine

insert :: Char -> ModifyBuffer
insert c buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusedLine %~ Z.push c

insertNewline :: ModifyBuffer
insertNewline buffer = modifyIORef' buffer $ \frozenBuffer -> do
    let lineRest = frozenBuffer^.focusedLine & Z.getRight
    frozenBuffer & focusedLine %~ Z.dropRight
                 & flushFocusedLine
                 & textLines %~ (\lines' -> Z.right lines' & Z.insert lineRest)
                 & focusLine 0

delete :: ModifyBuffer
delete buffer = modifyIORef' buffer $ \frozenBuffer ->
    if | getLinePos frozenBuffer == 0 ->
            frozenBuffer & flushFocusedLine & textLines %~ (\lines' ->
                case Z.safeCursor lines' of
                    Just line -> Z.delete lines' & Z.left & cursorAppend line
                    Nothing   -> Z.left lines') & refocusLine
       | otherwise ->
            frozenBuffer & focusedLine %~ Z.pop
    where
        cursorAppend line lines' = case Z.safeCursor lines' of
            Just line' -> Z.replace (line' `LL.append` line) lines'
            Nothing -> Z.insert line lines'
