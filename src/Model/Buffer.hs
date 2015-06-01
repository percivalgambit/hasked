module Model.Buffer where

import Data.ListLike.Zipper as Z

import Data.Functor ((<$>))
import Data.IORef (newIORef, modifyIORef', readIORef, IORef)
import Data.ListLike.Instances ()
import qualified Data.ListLike.IO as LLIO
import Data.Sequence (Seq)
import System.IO (Handle)

type Buffer       = Zipper (Seq Char)
type MBuffer      = IORef Buffer
type ModifyBuffer = MBuffer -> IO ()

newBuffer :: IO MBuffer
newBuffer = newIORef Z.empty

newBufferFromFile :: Handle -> IO MBuffer
newBufferFromFile file =  newIORef =<< Z.fromListLike <$> LLIO.hGetContents file

getBuffer :: MBuffer -> IO Buffer
getBuffer = readIORef

writeBufferToFile :: FilePath -> Buffer -> IO ()
writeBufferToFile path = LLIO.writeFile path . toListLike

bLeft, bRight :: ModifyBuffer
bLeft  = flip modifyIORef' Z.left
bRight = flip modifyIORef' Z.right

bInsert :: Char -> ModifyBuffer
bInsert c = flip modifyIORef' (Z.insert c)

bDelete :: ModifyBuffer
bDelete = flip modifyIORef' Z.delete
