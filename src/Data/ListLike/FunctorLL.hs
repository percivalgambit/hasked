{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- Author: Lee Ehudin
-- Defines a restricted Functor class that only operates over ListLike instances

module Data.ListLike.FunctorLL (FunctorLL(..)) where

import qualified Data.ListLike.Base as LL

-- Restricted Functor class that only operates over ListLike instances
class FunctorLL f where
    fmap :: LL.ListLike full' item => (full -> full') -> f full -> f full'

-- Functor is a superclass of FunctorLL, since any function that can map from
-- any two arbitrary values can certainly map from ListLike instances
instance {-# OVERLAPPABLE #-} Functor f => FunctorLL f where
    fmap = Prelude.fmap
