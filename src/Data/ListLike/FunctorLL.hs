{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ListLike.FunctorLL (FunctorLL(..)) where

import qualified Data.ListLike.Base as LL

class FunctorLL f where
    fmap :: LL.ListLike full' item => (full -> full') -> f full -> f full'

instance {-# OVERLAPPABLE #-} Functor f => FunctorLL f where
    fmap = Prelude.fmap
