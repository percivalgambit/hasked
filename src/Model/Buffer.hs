{-# LANGUAGE TemplateHaskell #-}

module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete) where

import qualified Data.ListLike.Zipper as Z

import Control.Lens.Getter (to)
import Control.Lens.Operators ((^.), (&), (.~), (%~))
import Control.Lens.TH (makeLenses)
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
    , _focusedLine :: Maybe FocusedLine
    , _linePos     :: Int
    } deriving Show
makeLenses ''Buffer
type MBuffer      = IORef Buffer
type ModifyBuffer = MBuffer -> IO ()

focusLine :: Buffer -> Buffer
focusLine buf = buf & focusedLine %~ \line -> case line of
    Just _  -> line
    Nothing -> Just $ buf^.textLines
                          .to Z.cursor
                          .to Z.zip
                          .to (iterate Z.right)
                          .to (!! (buf^.linePos))

flushFocusedLine :: Buffer -> Buffer
flushFocusedLine buf = case buf^.focusedLine of
    Just focus -> buf & textLines   .~ Z.replace (Z.unzip focus) (buf^.textLines)
                      & focusedLine .~ Nothing
    Nothing    -> buf

incLinePos :: Buffer -> Buffer
incLinePos buf = buf & linePos %~ \pos ->
    case buf^.focusedLine of
        Just line
            | pos >= Z.length line -> Z.length line
            | otherwise            -> pos + 1
        Nothing -> error "Model.Buffer.incLinePos: focusedLine is Nothing"

decLinePos :: Buffer -> Buffer
decLinePos buf = buf & linePos %~ \pos ->
    case buf^.focusedLine of
        Just line
            | pos == 0             -> 0
            | pos >= Z.length line -> Z.length line - 1
            | otherwise            -> pos - 1
        Nothing -> error "Model.Buffer.decLinePos: focusedLine is Nothing"

newBuffer :: IO MBuffer
newBuffer = newIORef $ Buffer Z.empty Nothing 0

newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = do
    fileLines <- Z.zip . LLS.lines <$> LLIO.readFile filepath
    newIORef $ Buffer fileLines Nothing 0

writeBufferToFile :: FilePath -> ModifyBuffer
writeBufferToFile filepath buffer = do
    frozenBuffer <- flushFocusedLine <$> readIORef buffer
    frozenBuffer^.textLines
                 .to Z.unzip
                 .to LLS.unlines
                 .to (LLIO.writeFile filepath)

left, right, upLine, downLine :: ModifyBuffer
left buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusLine & focusedLine %~ fmap Z.left & decLinePos

right buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusLine & focusedLine %~ fmap Z.right & incLinePos

upLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.left

downLine buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & flushFocusedLine & textLines %~ Z.right

insert :: Char -> ModifyBuffer
insert c buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusLine & focusedLine %~ fmap (Z.push c) & incLinePos

delete :: ModifyBuffer
delete buffer = modifyIORef' buffer $ \frozenBuffer ->
    frozenBuffer & focusLine & focusedLine %~ fmap Z.pop & decLinePos
