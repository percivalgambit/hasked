module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete) where

import qualified Data.ListLike.Zipper as Z

import Control.Category ((>>>))
import Control.Lens.Lens ((<&>))
import Data.IORef (newIORef, readIORef, IORef)
import Data.ListLike.Instances ()
import qualified Data.ListLike.IO as LLIO
import qualified Data.ListLike.String as LLS
import qualified Data.Sequence as S

type Line    = S.Seq Char
type Lines   = S.Seq Line
type Buffer  = Z.Zipper Lines
type MBuffer = IORef Buffer

newBuffer :: IO MBuffer
newBuffer = newIORef Z.empty

newBufferFromFile :: FilePath -> IO MBuffer
newBufferFromFile filepath = LLIO.readFile filepath
                             <&> (LLS.lines >>> Z.zip)
                             >>= newIORef

writeBufferToFile :: FilePath -> MBuffer -> IO ()
writeBufferToFile filepath buffer = readIORef buffer
                                    <&> (Z.unzip >>> LLS.unlines)
                                    >>= LLIO.writeFile filepath

left, right, upLine, downLine :: MBuffer -> IO ()
left = undefined
right = undefined
upLine = undefined
downLine = undefined

insert :: Char -> MBuffer -> IO ()
insert = undefined

delete :: MBuffer -> IO ()
delete = undefined
