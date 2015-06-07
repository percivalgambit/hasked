{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete,
                     setScreenSize, getScreen, getCursorPos, MBuffer) where

import qualified Data.ListLike.Zipper as Z

import Control.Lens.At (ix)
import Control.Lens.Getter (to, view)
import Control.Lens.Operators ((^.), (&), (<&>), (.~), (%~), (^?))
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_1, _2)
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
    , _focusedLine :: Maybe FocusedLine
    , _cursorPos   :: (Int, Int)
    , _screenSize  :: (Int, Int)
    } deriving Show
makeLenses ''Buffer
type MBuffer      = IORef Buffer
type ModifyBuffer = MBuffer -> IO ()

focusLine :: Buffer -> Buffer
focusLine buf = buf & focusedLine %~ \line -> case line of
    Just _  -> line
    Nothing -> buf^.textLines.to Z.safeCursor & (\focus ->
        case focus of
            Nothing -> Just Z.empty
            Just l  -> iterate Z.right (Z.zip l) ^? ix (buf^.cursorPos._2))

flushFocusedLine :: Buffer -> Buffer
flushFocusedLine buf = case buf^.focusedLine of
    Just focus
        | Z.endp focus ->
            buf & textLines   .~ Z.push (Z.unzip focus) (buf^.textLines)
                & focusedLine .~ Nothing
        | otherwise    ->
            buf & textLines   .~ Z.replace (Z.unzip focus) (buf^.textLines)
                & focusedLine .~ Nothing
    Nothing -> buf

incCursorXPos :: Buffer -> Buffer
incCursorXPos buf = buf & cursorPos._2 %~ \pos ->
    case buf^.focusedLine of
        Just line
            | pos >= Z.length line -> Z.length line
            | otherwise            -> pos + 1
        Nothing -> error "Model.Buffer.incCursorXPos: focusedLine is Nothing"

decCursorXPos :: Buffer -> Buffer
decCursorXPos buf = buf & cursorPos._2 %~ \pos ->
    case buf^.focusedLine of
        Just line
            | pos == 0             -> 0
            | pos >= Z.length line -> Z.length line - 1
            | otherwise            -> pos - 1
        Nothing -> error "Model.Buffer.decCursorXPos: focusedLine is Nothing"

incCursorYPos :: Buffer -> Buffer
incCursorYPos buf = buf & cursorPos._1 %~ \pos -> do
    let lines' = buf^.textLines
    if | pos >= Z.length lines' -> Z.length lines'
       | otherwise              -> pos + 1

decCursorYPos :: Buffer -> Buffer
decCursorYPos buf = buf & cursorPos._1 %~ \pos -> do
    let lines' = buf^.textLines
    if | pos == 0               -> 0
       | pos >= Z.length lines' -> Z.length lines' - 1
       | otherwise              -> pos - 1

newBuffer :: IO MBuffer
newBuffer = newIORef $ Buffer Z.empty Nothing (0, 0) (0, 0)

newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = do
    fileLines <- Z.zip . LLS.lines <$> LLIO.readFile filepath
    newIORef $ Buffer fileLines Nothing (0, 0) (0, 0)

writeBufferToFile :: FilePath -> ModifyBuffer
writeBufferToFile filepath buffer = do
    frozenBuffer <- flushFocusedLine <$> readIORef buffer
    frozenBuffer^.textLines
                 .to Z.unzip
                 .to LLS.unlines
                 .to (LLIO.writeFile filepath)

setScreenSize :: (Int, Int) -> ModifyBuffer
setScreenSize scrSize buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & screenSize .~ scrSize

getScreen :: MBuffer -> IO String
getScreen buffer = do
    frozenBuffer <- flushFocusedLine <$> readIORef buffer
    let scrSize = frozenBuffer^.screenSize
    return $ frozenBuffer^.textLines
                          .to Z.unzip
                              & LL.take (scrSize^._1)
                              & fmap (LL.take $ scrSize^._2)
                              & LLS.unlines
                              & LLS.toString

getCursorPos :: MBuffer -> IO (Int, Int)
getCursorPos buffer = readIORef buffer <&> view cursorPos

left, right, upLine, downLine :: ModifyBuffer
left buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusLine & focusedLine %~ fmap Z.left & decCursorXPos

right buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusLine & focusedLine %~ fmap Z.right & incCursorXPos

upLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.left & decCursorYPos

downLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.right & incCursorYPos

insert :: Char -> ModifyBuffer
insert c buffer
    | c == '\n' = modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & flushFocusedLine
                     & textLines %~ Z.insert S.empty
                     & incCursorYPos
    | otherwise = modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & focusLine & focusedLine %~ fmap (Z.push c) & incCursorXPos

delete :: ModifyBuffer
delete buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusLine & focusedLine %~ fmap Z.pop & decCursorXPos
