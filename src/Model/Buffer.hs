module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete) where

import qualified Data.ListLike.Zipper as Z

import Data.IORef (newIORef, IORef)
import Data.ListLike.Instances ()
import Data.Sequence (Seq)

type Line   = Seq Char
type Buffer = Z.Zipper (Seq Line)

newBuffer :: IO (IORef Buffer)
newBuffer = newIORef Z.empty

newBufferFromFile :: FilePath -> IO (IORef Buffer)
newBufferFromFile = undefined

writeBufferToFile :: FilePath -> IORef Buffer -> IO ()
writeBufferToFile = undefined

left, right, upLine, downLine :: IORef Buffer -> IO ()
left = undefined
right = undefined
upLine = undefined
downLine = undefined

insert :: Char -> IORef Buffer -> IO ()
insert = undefined

delete :: IORef Buffer -> IO ()
delete = undefined
