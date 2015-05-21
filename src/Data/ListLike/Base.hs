{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Copied from ListLike on Hackage to make a more flexible version by
-- including a list type in the typeclass instead of a full type

module Data.ListLike.Base (ListLike(..)) where

import Prelude hiding (null, head, tail, map, reverse)

import qualified Data.List as List

class (Monoid (list item)) => ListLike list item where

    empty :: list item
    empty = mempty

    null :: list item -> Bool

    singleton :: item -> list item

    append :: list item -> list item -> list item
    append = mappend

    cons :: item -> list item -> list item
    cons = append . singleton

    snoc :: list item -> item -> list item
    snoc l = append l . singleton

    head :: list item -> item
    tail :: list item -> list item

    map :: (ListLike list' item') => (item -> item') -> list item -> list' item'
    map func inp
        | null inp  = empty
        | otherwise = cons (func $ head inp) (map func $ tail inp)

    reverse :: list item -> list item
    reverse l = rev l empty
        where rev rl a
                | null rl = a
                | otherwise = rev (tail rl) (cons (head rl) a)

    toList :: list item -> [item]
    toList = fromListLike

    fromList :: [item] -> list item
    fromList []     = empty
    fromList (x:xs) = cons x $ fromList xs

    fromListLike :: (ListLike list' item) => list item -> list' item
    fromListLike = map id

instance ListLike [] a where
    empty = []
    singleton x = [x]
    cons = (:)
    snoc l x = l ++ [x]
    append = (++)
    head = List.head
    tail = List.tail
    null = List.null
    map f = fromList . List.map f
    reverse = List.reverse
    toList = id
    fromList = id
