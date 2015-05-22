module Model.Buffer where

import Data.ListLike.Zipper as Z

import Data.Functor
import Data.IORef
import qualified Data.ListLike.IO as LLIO
import Data.Sequence
import System.IO

type Buffer       = Zipper (Seq Char)
type MBuffer      = IORef Buffer
type ModifyBuffer = MBuffer -> IO ()

newBuffer :: IO MBuffer
newBuffer = newIORef Z.empty

newBufferFromFile :: Handle -> IO MBuffer
newBufferFromFile file =  newIORef =<< Z.fromListLike <$> LLIO.hGetContents file

getBuffer :: MBuffer -> IO Buffer
getBuffer = readIORef

bLeft, bRight :: ModifyBuffer
bLeft  = flip modifyIORef' Z.left
bRight = flip modifyIORef' Z.right

bInsert :: Char -> ModifyBuffer
bInsert c = flip modifyIORef' (Z.insert c)

bDelete :: ModifyBuffer
bDelete = flip modifyIORef' Z.delete
