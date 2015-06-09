{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Author: Lee Ehudin
-- Contains the polymorphic Zipper type for ListLike instances

-- Copied from ListZipper on Hackage to make a more genric version
-- https://hackage.haskell.org/package/ListZipper

module Data.ListLike.Zipper (Zipper, empty, fromListLike, fromListLikeEnd,
                             toListLike, zip, zipEnd, unzip, beginp, endp,
                             emptyp, length, position, getLeft, getRight, dropLeft,
                             dropRight, start, end, cursor, safeCursor, left,
                             right, insert, delete, push, pop, replace, reversez,
                             foldrz, foldlz, foldlz') where

import Prelude hiding (length, zip, unzip)

import qualified Data.ListLike.FunctorLL as FLL

import qualified Data.ListLike.Base as LL

import Control.Lens.Empty (AsEmpty(..))
import Control.Lens.Prism (nearly)
import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))

-- Polymorphic Zipper type that represents a zipper with a focus on the head
-- of the right sub-list
data Zipper full = forall item. LL.ListLike full item => Zip !full !full

deriving instance Eq full => Eq (Zipper full)
deriving instance Show full => Show (Zipper full)

instance (LL.ListLike full item, Arbitrary full) => Arbitrary (Zipper full) where
    arbitrary = liftM2 Zip arbitrary arbitrary

    shrink (Zip ls rs) = [Zip ls' rs | ls' <- shrink ls]
                    ++ [Zip ls rs' | rs' <- shrink rs]

instance CoArbitrary full => CoArbitrary (Zipper full) where
    coarbitrary (Zip ls rs) = coarbitrary rs . coarbitrary ls

instance LL.ListLike full item => AsEmpty (Zipper full) where
    _Empty = nearly empty emptyp

instance FLL.FunctorLL Zipper where
    fmap f (Zip ls rs) = Zip (f ls) (f rs)

-- Returns an instance of a Zipper wrapping an empty ListLike type
empty :: (LL.ListLike full item) => Zipper full
empty = Zip LL.empty LL.empty

-- Converts a ListLike into a Zipper with a focus on the first element
fromListLike :: (LL.ListLike full item) => full -> Zipper full
fromListLike = Zip LL.empty

-- Converts a ListLike into a Zipper with a focus just past the end
fromListLikeEnd :: (LL.ListLike full item) => full -> Zipper full
fromListLikeEnd as = Zip (LL.reverse as) LL.empty

-- Converts a Zipper into the underlying ListLike type
toListLike :: Zipper full -> full
toListLike (Zip ls rs) = LL.reverse ls `LL.append` rs

-- Alias for fromListLike
zip :: (LL.ListLike full item) => full -> Zipper full
zip = fromListLike

-- Alias for fromListLikeEnd
zipEnd :: (LL.ListLike full item) => full -> Zipper full
zipEnd = fromListLikeEnd

-- Alias for toListLike
unzip :: Zipper full -> full
unzip = toListLike

-- Returns True if the focus of the Zipper is at the first element of the
-- underlying ListLike, false otherwise
beginp :: Zipper full -> Bool
beginp (Zip ls _) = LL.null ls

-- Returns True if the focus of the Zipper is just past the end of the underlying
-- ListLike, false otherwise
endp :: Zipper full -> Bool
endp (Zip _ rs) = LL.null rs

-- Returns True if there are no elements in the Zipper
emptyp :: Zipper full -> Bool
emptyp (Zip ls rs) = LL.null ls && LL.null rs

-- Returns the number of elements stored in the Zipper
length :: Zipper full -> Int
length (Zip ls rs) = LL.length ls + LL.length rs

-- Returns the position of the focus from the front of the ListLike.  This is
-- equivalent to the number of elements to the left of the focus.
position :: Zipper full -> Int
position (Zip ls _) = LL.length ls

-- getLeft returns all of the elements to the left of the focus in the underlying
-- ListLike type, and getRight returns all of the elements to the right
getLeft, getRight :: Zipper full -> full
getLeft  (Zip ls _) = LL.reverse ls
getRight (Zip _ rs) = rs

-- dropLeft removes all of the elements to the left of the focus, and dropRight
-- removes all of the elements to the right
dropLeft, dropRight :: Zipper full -> Zipper full
dropLeft  (Zip _ rs) = Zip LL.empty rs
dropRight (Zip ls _) = Zip ls LL.empty

-- start moves the focus to the beginning of the underlying ListLike, and end
-- moves the focus just past the end
start, end :: Zipper full -> Zipper full
start (Zip ls rs) = Zip LL.empty (LL.reverse ls `LL.append` rs)
end   (Zip ls rs) = Zip (LL.reverse rs `LL.append` ls) LL.empty

-- Returns the element at the focus of the Zipper.  This operation is unsafe and
-- will throw an error if the focus is just past the end of the underlying ListLike
cursor :: (LL.ListLike full item) => Zipper full -> item
cursor (Zip _ rs)  = LL.head rs

-- A safe version of cursor.  This wraps the result in a Maybe monad, so if the
-- focus is just past the end of the underlying ListLike, the result is Nothing,
-- otherwise the result is Just the element at the focus
safeCursor :: (LL.ListLike full item) => Zipper full -> Maybe item
safeCursor (Zip _ rs)
    | LL.null rs = Nothing
    | otherwise  = Just (LL.head rs)

-- Moves the focus one element to the left.  If the focus is at the beginning of
-- the ListLike, it will not move.
left :: Zipper full -> Zipper full
left z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) (LL.head ls `LL.cons` rs)

-- Moves the focus one element to the right.  If the focus is just past the end of
-- the ListLike, it will not move.
right :: Zipper full -> Zipper full
right z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip (LL.head rs `LL.cons` ls) (LL.tail rs)

-- Insert an element at the focus of the Zipper
insert :: (LL.ListLike full item) => item -> Zipper full -> Zipper full
insert a (Zip ls rs) = Zip ls (a `LL.cons` rs)

-- Delete the element at the focus of the Zipper.  If the focus is just past the
-- end of the underlying ListLike, nothing will happen.
delete :: Zipper full -> Zipper full
delete z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (LL.tail rs)

-- Push an element one space to the left of the focus.
push :: (LL.ListLike full item) => item -> Zipper full -> Zipper full
push a (Zip ls rs) = Zip (a `LL.cons` ls) rs

-- Delete the element one space to the left of the focus of the Zipper.  If the
-- focus is at the beginning of the underlying ListLike, nothing will happen.
pop :: Zipper full -> Zipper full
pop z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) rs

-- Replace the element at the focus of the Zipper. If the focus is just past the
-- end of the underlying ListLike, nothing will happen.
replace :: (LL.ListLike full item) => item -> Zipper full -> Zipper full
replace a z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (a `LL.cons` rsTail) where
        rsTail = LL.tail rs

-- reverse the order of the elements in the Zipper
reversez :: Zipper full -> Zipper full
reversez (Zip ls rs) = Zip rs ls

-- Fold a function over all of the elements to the right of the focus.
foldrz :: (Zipper full -> b -> b) -> b -> Zipper full -> b
foldrz f x = go where
    go z
        | endp z    = x
        | otherwise = f z (go $ right z)

-- Fold a function over all of the elements to the right of the focus.
foldlz :: (b -> Zipper full -> b) -> b -> Zipper full -> b
foldlz f x z
        | endp z    = x
        | otherwise = foldlz f (f x z) (right z)

-- Fold a function over all of the elements to the right of the focus, with a
-- strict accumulator
foldlz' :: (b -> Zipper full -> b) -> b -> Zipper full -> b
foldlz' f x z
        | endp z    = x
        | otherwise = acc `seq` foldlz' f acc (right z)
        where acc = f x z
