module Model.Buffer where

import Data.ListLike.Zipper

import Data.IORef
import Data.Sequence

type Buffer = IORef (Zipper (Seq Char))
