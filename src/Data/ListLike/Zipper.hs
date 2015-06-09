{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Copied from ListZipper on Hackage to make a more genric version
-- https://hackage.haskell.org/package/ListZipper

module Data.ListLike.Zipper (Zipper, empty, fromListLike, fromListLikeEnd,
                             toListLike, zip, zipEnd, unzip, beginp, endp,
                             emptyp, length, position, getLeft, getRight, dropLeft,
                             dropRight, start, end, cursor, safeCursor, left,
                             right, insert, delete, push, pop, replace, reversez,
                             foldrz, foldlz, foldlz') where

import Prelude hiding (zip, unzip, length)

import qualified Data.ListLike.FunctorLL as FLL

import qualified Data.ListLike.Base as LL

import Control.Monad (liftM2)
import Control.Lens.Empty (AsEmpty(..))
import Control.Lens.Prism (nearly)
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))

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

empty :: (LL.ListLike full item) => Zipper full
empty = Zip LL.empty LL.empty

fromListLike :: (LL.ListLike full item) => full -> Zipper full
fromListLike = Zip LL.empty

fromListLikeEnd :: (LL.ListLike full item) => full -> Zipper full
fromListLikeEnd as = Zip (LL.reverse as) LL.empty

toListLike :: Zipper full -> full
toListLike (Zip ls rs) = LL.reverse ls `LL.append` rs

zip :: (LL.ListLike full item) => full -> Zipper full
zip = fromListLike

zipEnd :: (LL.ListLike full item) => full -> Zipper full
zipEnd = fromListLikeEnd

unzip :: Zipper full -> full
unzip = toListLike

beginp :: Zipper full -> Bool
beginp (Zip ls _) = LL.null ls

endp :: Zipper full -> Bool
endp (Zip _ rs) = LL.null rs

emptyp :: Zipper full -> Bool
emptyp (Zip ls rs) = LL.null ls && LL.null rs

length :: Zipper full -> Int
length (Zip ls rs) = LL.length ls + LL.length rs

position :: Zipper full -> Int
position (Zip ls _) = LL.length ls

getLeft, getRight :: Zipper full -> full
getLeft  (Zip ls _) = LL.reverse ls
getRight (Zip _ rs) = rs

dropLeft, dropRight :: Zipper full -> Zipper full
dropLeft  (Zip _ rs) = Zip LL.empty rs
dropRight (Zip ls _) = Zip ls LL.empty

start, end :: Zipper full -> Zipper full
start (Zip ls rs) = Zip LL.empty (LL.reverse ls `LL.append` rs)
end   (Zip ls rs) = Zip (LL.reverse rs `LL.append` ls) LL.empty

cursor :: (LL.ListLike full item) => Zipper full -> item
cursor (Zip _ rs)  = LL.head rs

safeCursor :: (LL.ListLike full item) => Zipper full -> Maybe item
safeCursor (Zip _ rs)
    | LL.null rs = Nothing
    | otherwise  = Just (LL.head rs)

left :: Zipper full -> Zipper full
left z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) (LL.head ls `LL.cons` rs)

right :: Zipper full -> Zipper full
right z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip (LL.head rs `LL.cons` ls) (LL.tail rs)

insert :: (LL.ListLike full item) => item -> Zipper full -> Zipper full
insert a (Zip ls rs) = Zip ls (a `LL.cons` rs)

delete :: Zipper full -> Zipper full
delete z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (LL.tail rs)

push :: (LL.ListLike full item) => item -> Zipper full -> Zipper full
push a (Zip ls rs) = Zip (a `LL.cons` ls) rs

pop :: Zipper full -> Zipper full
pop z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) rs

replace :: (LL.ListLike full item) => item -> Zipper full -> Zipper full
replace a z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (a `LL.cons` rsTail) where
        rsTail = LL.tail rs

reversez :: Zipper full -> Zipper full
reversez (Zip ls rs) = Zip rs ls

foldrz :: (Zipper full -> b -> b) -> b -> Zipper full -> b
foldrz f x = go where
    go z
        | endp z    = x
        | otherwise = f z (go $ right z)

foldlz :: (b -> Zipper full -> b) -> b -> Zipper full -> b
foldlz f x z
        | endp z    = x
        | otherwise = foldlz f (f x z) (right z)

foldlz' :: (b -> Zipper full -> b) -> b -> Zipper full -> b
foldlz' f x z
        | endp z    = x
        | otherwise = acc `seq` foldlz' f acc (right z)
        where acc = f x z
