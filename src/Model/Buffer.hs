{-# LANGUAGE TemplateHaskell #-}

module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete) where

import qualified Data.ListLike.Zipper as Z

import Control.Lens.Extras (is)
import Control.Lens.Getter (to)
import Control.Lens.Operators ((^.), (&), (.~), (%~), (-~), (+~))
import Control.Lens.Prism (matching, _Nothing, _Just)
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

focusLine :: ModifyBuffer
focusLine = flip modifyIORef' $ \frozenBuffer ->
    frozenBuffer & focusedLine %~ \line ->
        if is _Nothing line
        then Just $ frozenBuffer^.textLines
                                 .to Z.cursor
                                 .to Z.zip
                                 .to (iterate Z.right)
                                 .to (!! (frozenBuffer^.linePos))
        else line

flushFocusedLine :: ModifyBuffer
flushFocusedLine = flip modifyIORef' $ \frozenBuffer ->
    case matching _Just $ frozenBuffer^.focusedLine of
        Right focus -> frozenBuffer & textLines .~ Z.replace (Z.unzip focus)
                                                             (frozenBuffer^.textLines)
                                    & focusedLine .~ Nothing
        Left  _     -> frozenBuffer

newBuffer :: IO MBuffer
newBuffer = newIORef $ Buffer Z.empty Nothing 0

newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = do
    fileLines <- Z.zip . LLS.lines <$> LLIO.readFile filepath
    newIORef $ Buffer fileLines Nothing 0

writeBufferToFile :: FilePath -> ModifyBuffer
writeBufferToFile filepath buffer = do
    flushFocusedLine buffer
    frozenBuffer <- readIORef buffer
    frozenBuffer^.textLines
                 .to Z.unzip
                 .to LLS.unlines
                 .to (LLIO.writeFile filepath)

left, right, upLine, downLine :: ModifyBuffer
left buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & focusedLine %~ fmap Z.left
                     & linePos -~ 1

right buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & focusedLine %~ fmap Z.right
                     & linePos +~ 1

upLine buffer = do
    flushFocusedLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & textLines %~ Z.left

downLine buffer = do
    flushFocusedLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & textLines %~ Z.right

insert :: Char -> ModifyBuffer
insert c buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & focusedLine %~ fmap (Z.insert c)
                     & linePos +~ 1

delete :: ModifyBuffer
delete buffer = do
    focusLine buffer
    modifyIORef' buffer $ \frozenBuffer ->
        frozenBuffer & focusedLine %~ fmap Z.delete
                     & linePos -~ 1
