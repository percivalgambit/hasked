{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Zipper where

import qualified Data.ListLike.Base as LL
import Data.ListLike.Instances

import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))

data Zipper full item = forall item. LL.ListLike full item => Zip !full !full

deriving instance Eq full => Eq (Zipper full item)
deriving instance Show full => Show (Zipper full item)

instance (Arbitrary full, LL.ListLike full item) => Arbitrary (Zipper full item) where
    arbitrary = liftM2 Zip arbitrary arbitrary
    shrink (Zip ls rs) = [Zip ls' rs | ls' <- shrink ls]
                    ++ [Zip ls rs' | rs' <- shrink rs]

instance CoArbitrary full => CoArbitrary (Zipper full item) where
    coarbitrary (Zip ls rs) = coarbitrary rs . coarbitrary ls

empty :: (LL.ListLike full item) => Zipper full item
empty = Zip LL.empty LL.empty

fromListLike :: (LL.ListLike full item) => full -> Zipper full item
fromListLike = Zip LL.empty

fromListLikeEnd :: (LL.ListLike full item) => full -> Zipper full item
fromListLikeEnd as = Zip (LL.reverse as) LL.empty

toList :: (LL.ListLike full item) => Zipper full item -> [item]
toList (Zip ls rs) = LL.toList $ LL.reverse ls `LL.append` rs

beginp :: Zipper full item -> Bool
beginp (Zip ls _) = LL.null ls

endp :: Zipper full item -> Bool
endp (Zip _ rs) = LL.null rs

emptyp :: Zipper full item -> Bool
emptyp (Zip ls rs) = LL.null ls && LL.null rs

start, end :: Zipper full item -> Zipper full item
start (Zip ls rs) = Zip LL.empty (LL.reverse ls `LL.append` rs)
end   (Zip ls rs) = Zip (LL.reverse rs `LL.append` ls) LL.empty

cursor :: (LL.ListLike full item) => Zipper full item -> item
cursor (Zip _ rs)  = LL.head rs

safeCursor :: (LL.ListLike full item) => Zipper full item -> Maybe item
safeCursor (Zip _ rs)
    | LL.null rs = Nothing
    | otherwise  = Just (LL.head rs)

left :: Zipper full item -> Zipper full item
left z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) (LL.head ls `LL.cons` rs)

right :: Zipper full item -> Zipper full item
right z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip (LL.head rs `LL.cons` ls) (LL.tail rs)

insert :: (LL.ListLike full item) => item -> Zipper full item -> Zipper full item
insert a (Zip ls rs) = Zip ls (a `LL.cons` rs)

delete :: Zipper full item -> Zipper full item
delete z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (LL.tail rs)

push :: (LL.ListLike full item) => item -> Zipper full item -> Zipper full item
push a (Zip ls rs) = Zip (a `LL.cons` ls) rs

pop :: Zipper full item -> Zipper full item
pop z@(Zip ls rs)
    | LL.null ls = z
    | otherwise  = Zip (LL.tail ls) rs

replace :: (LL.ListLike full item) => item -> Zipper full item -> Zipper full item
replace a z@(Zip ls rs)
    | LL.null rs = z
    | otherwise  = Zip ls (a `LL.cons` rsTail) where
        rsTail = LL.tail rs

reversez :: Zipper full item -> Zipper full item
reversez (Zip ls rs) = Zip rs ls

foldrz :: (Zipper full item -> b -> b) -> b -> Zipper full item -> b
foldrz f x = go where
    go z
        | endp z    = x
        | otherwise = f z (go $ right z)

foldlz :: (b -> Zipper full item -> b) -> b -> Zipper full item -> b
foldlz f x z
        | endp z    = x
        | otherwise = foldlz f (f x z) (right z)

foldlz' :: (b -> Zipper full item -> b) -> b -> Zipper full item -> b
foldlz' f x z
        | endp z    = x
        | otherwise = acc `seq` foldlz' f acc (right z)
        where acc = f x z
