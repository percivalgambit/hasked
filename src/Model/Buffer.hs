module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete) where

import qualified Data.ListLike.Zipper as Z

import Data.IORef (newIORef, IORef)
import Data.ListLike.Instances ()
import qualified Data.Sequence as S

type Line    = S.Seq Char
type Lines   = S.Seq Line
type Buffer  = Z.Zipper Lines
type MBuffer = IORef Buffer

newBuffer :: IO MBuffer
newBuffer = newIORef Z.empty

newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = newIORef
                             =<< Z.zip . S.fromList . seqLines
                             <$> readFile filepath where
    seqLines = (fmap . fmap) S.fromList lines

writeBufferToFile :: FilePath -> MBuffer -> IO ()
writeBufferToFile = undefined

left, right, upLine, downLine :: MBuffer -> IO ()
left = undefined
right = undefined
upLine = undefined
downLine = undefined

insert :: Char -> MBuffer -> IO ()
insert = undefined

delete :: MBuffer -> IO ()
delete = undefined
