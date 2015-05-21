{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

-- Copied from ListZipper on Hackage to make a more genric version

module Data.ListLike.Zipper where

import qualified Data.ListLike.Base as LL

import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))

data Zipper list item where
    Zip :: LL.ListLike list item =>
           !(list item) -> !(list item) -> Zipper list item

deriving instance Eq (list item) => Eq (Zipper list item)
deriving instance Show (list item) => Show (Zipper list item)

instance (Arbitrary (list item), LL.ListLike list item) => Arbitrary (Zipper list item) where
    arbitrary = liftM2 Zip arbitrary arbitrary
    shrink (Zip ls rs) = [Zip ls' rs | ls' <- shrink ls]
                    ++ [Zip ls rs' | rs' <- shrink rs]

instance CoArbitrary (list item) => CoArbitrary (Zipper list item) where
    coarbitrary (Zip ls rs) = coarbitrary rs . coarbitrary ls


empty :: (LL.ListLike list item) => Zipper list item
empty = Zip LL.empty LL.empty

fromListLike :: (LL.ListLike list item) => (list item) -> Zipper list item
fromListLike = Zip LL.empty

fromListLikeEnd :: (LL.ListLike list item) => (list item) -> Zipper list item
fromListLikeEnd as = Zip (LL.reverse as) LL.empty

toList :: (LL.ListLike list item) => Zipper list item -> [item]
toList (Zip ls rs) = LL.toList $ LL.reverse ls `LL.append` rs

beginp :: Zipper list item -> Bool
beginp (Zip ls _) = LL.null ls

endp :: Zipper list item -> Bool
endp (Zip _ rs) = LL.null rs

emptyp :: Zipper list item -> Bool
emptyp (Zip ls rs) = LL.null ls && LL.null rs

start, end :: Zipper list item -> Zipper list item
start (Zip ls rs) = Zip LL.empty (LL.reverse ls `LL.append` rs)
end   (Zip ls rs) = Zip (LL.reverse rs `LL.append` ls) LL.empty

cursor :: (LL.ListLike list item) => Zipper list item -> item
cursor (Zip _ rs)  = LL.head rs

safeCursor :: (LL.ListLike list item) => Zipper list item -> Maybe item
safeCursor (Zip _ rs)
    | LL.null rs = Nothing
    | otherwise  = Just (LL.head rs)

left :: Zipper list item -> Zipper list item
left z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) (LL.head ls `LL.cons` rs)

right :: Zipper list item -> Zipper list item
right z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip (LL.head rs `LL.cons` ls) (LL.tail rs)

insert :: (LL.ListLike list item) => item -> Zipper list item -> Zipper list item
insert a (Zip ls rs) = Zip ls (a `LL.cons` rs)

delete :: Zipper list item -> Zipper list item
delete z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (LL.tail rs)

push :: (LL.ListLike list item) => item -> Zipper list item -> Zipper list item
push a (Zip ls rs) = Zip (a `LL.cons` ls) rs

pop :: Zipper list item -> Zipper list item
pop z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) rs

replace :: (LL.ListLike list item) => item -> Zipper list item -> Zipper list item
replace a z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (a `LL.cons` rsTail) where
        rsTail = LL.tail rs

reversez :: Zipper list item -> Zipper list item
reversez (Zip ls rs) = Zip rs ls

foldrz :: (Zipper list item -> b -> b) -> b -> Zipper list item -> b
foldrz f x = go where
    go z
        | endp z    = x
        | otherwise = f z (go $ right z)

foldlz :: (b -> Zipper list item -> b) -> b -> Zipper list item -> b
foldlz f x z
        | endp z    = x
        | otherwise = foldlz f (f x z) (right z)

foldlz' :: (b -> Zipper list item -> b) -> b -> Zipper list item -> b
foldlz' f x z
        | endp z    = x
        | otherwise = acc `seq` foldlz' f acc (right z)
        where acc = f x z
