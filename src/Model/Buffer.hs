{-# LANGUAGE Rank2Types #-}

module Model.Buffer (newBuffer, newBufferFromFile, writeBufferToFile,
                     left, right, upLine, downLine, insert, delete) where

import qualified Data.ListLike.Zipper as Z

import Control.Category ((>>>))
import Control.Lens.Lens ((&), (<&>), Lens')
import Data.IORef (newIORef, readIORef, IORef)
import Data.ListLike.Instances ()
import qualified Data.ListLike.IO as LLIO
import qualified Data.ListLike.String as LLS
import qualified Data.Sequence as S

type Line        = S.Seq Char
type FocusedLine = Z.Zipper Line
type Lines       = S.Seq Line
type Buffer      = Z.Zipper Lines
type MBuffer     = IORef Buffer

focusLine :: Lens' Buffer FocusedLine
-- focusLine :: Functor f => (FocusedLine -> f FocusedLine) -> Buffer -> f Buffer
focusLine focus_fn buffer = focus_fn focus <&> unfocusLine where
    unfocusLine focus' = Z.unzip focus' & flip Z.replace buffer
    focus              = Z.cursor buffer & Z.zip

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

left, right, upLine, downLine :: Lens' Buffer FocusedLine
-- :: Functor f => (FocusedLine -> f FocusedLine) -> Buffer -> f Buffer
left focus_fn = focusLine (focus_fn >>> fmap Z.left)
right focus_fn = focusLine (focus_fn >>> fmap Z.right)
upLine focus_fn = focusLine focus_fn . Z.left
downLine focus_fn = focusLine focus_fn . Z.right

insert :: Char -> Lens' Buffer FocusedLine
-- insert :: Functor f => (FocusedLine -> f FocusedLine) -> Buffer -> f Buffer
insert c focus_fn = focusLine (focus_fn <$> Z.insert c)

delete :: Lens' Buffer FocusedLine
-- delete :: Functor f => (FocusedLine -> f FocusedLine) -> Buffer -> f Buffer
delete focus_fn = focusLine (focus_fn >>> fmap Z.delete)
