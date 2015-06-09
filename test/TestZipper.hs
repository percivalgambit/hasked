{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module TestZipper (zipperTests) where

import qualified Data.ListLike.Zipper as Z

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Modifiers (NonNegative(..), Small(..))

import Control.Exception (evaluate)
import Control.Lens.Empty (_Empty)
import Control.Lens.Extras (is)
import qualified Data.ListLike.Base as LL

zipRight :: LL.ListLike full item => Int -> full -> Z.Zipper full
zipRight i a = iterate Z.right (Z.fromListLike a) !! i

zipLeft :: LL.ListLike full item => Int -> full -> Z.Zipper full
zipLeft i a = iterate Z.left (Z.fromListLikeEnd a) !! i

zipperTests :: Spec
zipperTests = describe "The Zipper data type" $ do
    prop "recognizes an empty instance with the _Empty lens" $
        \(z :: Z.Zipper String) ->
            is _Empty z `shouldBe` (z == Z.empty || Z.emptyp z)

    prop "toListLike reverses fromListLike and fromListLikeEnd" $
        \(a :: String) -> do
            (Z.toListLike $ Z.fromListLike a) `shouldBe` a
            (Z.toListLike $ Z.fromListLikeEnd a) `shouldBe` a

    it "includes an empty function that i the same thing as creating a\
       \ Zipper from an empty ListLike type" $
        Z.fromListLike (LL.empty :: String) `shouldBe` Z.empty

    prop "zip and unzip are synonyms for fromListLike and toListLike" $
        \(a :: String) (z :: Z.Zipper String) -> do
            Z.zip a `shouldBe` Z.fromListLike a
            Z.unzip z `shouldBe` Z.toListLike z

    prop "fromListLike will start a zipper at the beginning of the ListLike" $
        \(a :: String) -> Z.fromListLike a `shouldSatisfy` Z.beginp

    prop "fromListLikeEnd will start a zipper at the end of the ListLike" $
        \(a :: String) -> Z.fromListLikeEnd a `shouldSatisfy` Z.endp

    prop "emptyp tests for an empty Zipper" $ \(z :: Z.Zipper String) ->
        Z.emptyp z `shouldBe` (z == Z.empty)

    prop "the length of a Zipper is just the length of the underlying ListLike" $
        \(a :: String) -> Z.length (Z.fromListLike a) `shouldBe` LL.length a

    prop "the position of a Zipper is where the focus is located in the\
         \ underlying ListLike" $
            \(a :: String) (i :: NonNegative (Small Int)) -> do
                let ix = getSmall $ getNonNegative i

                let z = zipRight ix a
                Z.position z `shouldBe` min (LL.length a) ix

                let z' = zipLeft ix a
                Z.position z' `shouldBe` max 0 (LL.length a - ix)

    prop "combining the right side of the zipper with the left side is the same\
         \ as getting the underlying ListLike" $ \(z :: Z.Zipper String) ->
            Z.getLeft z `LL.append` Z.getRight z `shouldBe` Z.toListLike z

    prop "dropLeft is the same thing as dropping all of the elements before the\
         \ current position of the focus" $
            \(a :: String) (i :: NonNegative (Small Int)) -> do
                let ix = getSmall $ getNonNegative i

                let z = zipRight ix a
                Z.toListLike (Z.dropLeft z) `shouldBe` LL.drop ix a

                let z' = zipLeft ix a
                Z.toListLike (Z.dropLeft z')
                    `shouldBe` LL.drop (LL.length a - ix) a

    prop "dropRight is the same thing as dropping all of the elements after the\
         \ current position of the focus" $
            \(a :: String) (i :: NonNegative (Small Int)) -> do
                let ix = getSmall $ getNonNegative i

                let z = zipRight ix a
                Z.toListLike (Z.dropRight z) `shouldBe` LL.take ix a

                let z' = zipLeft ix a
                Z.toListLike (Z.dropRight z')
                    `shouldBe` LL.take (LL.length a - ix) a

    prop "start will move the zipper to the beginning" $
        \(z :: Z.Zipper String) -> Z.start z `shouldSatisfy` Z.beginp

    prop "end will move the zipper to the end" $
        \(z :: Z.Zipper String) -> Z.end z `shouldSatisfy` Z.endp

    prop "cursor will return the element currently at the focus, but will throw\
         \ an error if the focus is off the end of the list" $
            \(a :: String) (i :: NonNegative (Small Int)) -> do
                let ix = getSmall $ getNonNegative i

                let z = zipRight ix a
                if | Z.endp z  -> evaluate (Z.cursor z) `shouldThrow` anyErrorCall
                   | otherwise -> Z.cursor z `shouldBe` LL.index a ix

    prop "maybeCursor is a safe version of cursor, and will wrap the result in\
         \ a Maybe monad" $ \(a :: String) (i :: NonNegative (Small Int)) -> do
            let ix = getSmall $ getNonNegative i

            let z = zipRight ix a
            if | Z.endp z  -> Z.safeCursor z `shouldBe` Nothing
               | otherwise -> Z.safeCursor z `shouldBe` Just (LL.index a ix)

    prop "left and right will not move past the end of a Zipper" $
        \(a :: String) -> do
            zipRight (LL.length a + 1) a `shouldSatisfy` Z.endp
            zipLeft (LL.length a + 1) a `shouldSatisfy` Z.beginp

    prop "delete is the reverse of insert" $
        \(z :: Z.Zipper String) (c :: Char) -> Z.delete (Z.insert c z) `shouldBe` z

    prop "pop is the reverse of push" $
        \(z :: Z.Zipper String) (c :: Char) -> Z.pop (Z.push c z) `shouldBe` z

    prop "insert will add an element that will become the new cursor" $
        \(z :: Z.Zipper String) (c :: Char) -> Z.cursor (Z.insert c z) `shouldBe` c

    prop "push will add an element before the current cursor" $
        \(z :: Z.Zipper String) (c :: Char) ->
            (Z.cursor $ Z.left $ Z.push c z) `shouldBe` c

    prop "replace will replace the current focus with an element, if the current\
         \ focus is not off the end of the ListLike" $
            \(z :: Z.Zipper String) (c :: Char) ->
                if | Z.endp z  -> return ()
                   | otherwise -> Z.cursor (Z.replace c z) `shouldBe` c

    prop "reverse will reverse the underlying ListLike type" $
        \(z :: Z.Zipper String) ->
            (Z.toListLike $ Z.reversez z) `shouldBe` (LL.reverse $ Z.toListLike z)
