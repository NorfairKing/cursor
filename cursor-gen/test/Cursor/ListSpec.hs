{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.ListSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Control.Monad

import Cursor.List
import Cursor.List.Gen ()

spec :: Spec
spec = do
    eqSpec @(ListCursor Int)
    functorSpec @ListCursor
    genValidSpec @(ListCursor Double)
    describe "emptyListCursor" $
        it "is valid" $ shouldBeValid (emptyListCursor :: ListCursor Int)
    describe "makeListCursor" $
        it "produces valid list cursors" $
        producesValidsOnValids (makeListCursor :: [Double] -> ListCursor Double)
    describe "makeListCursorWithSelection" $
        it "produces valid list cursors" $
        producesValidsOnValids2
            (makeListCursorWithSelection :: Int -> [Double] -> ListCursor Double)
    describe "rebuildListCursor" $ do
        it "produces valid lists" $
            producesValidsOnValids
                (rebuildListCursor :: ListCursor Double -> [Double])
        it "is the inverse of makeListCursor" $
            inverseFunctions
                (makeListCursor :: [Int] -> ListCursor Int)
                rebuildListCursor
        it "is the inverse of makeListCursorWithSelection for any index" $
            forAllUnchecked $ \i ->
                inverseFunctions
                    (makeListCursorWithSelection i :: [Int] -> ListCursor Int)
                    rebuildListCursor
    describe "listCursorSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (listCursorSelectPrev :: ListCursor Double -> Maybe (ListCursor Double))
        it "is a movement" $ isMovementM listCursorSelectPrev
    describe "listCursorSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (listCursorSelectNext :: ListCursor Double -> Maybe (ListCursor Double))
        it "is a movement" $ isMovementM listCursorSelectNext
    describe "listCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2
                (listCursorSelectIndex :: Int -> ListCursor Double -> ListCursor Double)
        it "is a movement" $
            forAllUnchecked $ \ix -> isMovement (listCursorSelectIndex ix)
    describe "listCursorPrevItem" $
        it "produces valid items" $
        producesValidsOnValids
            (listCursorPrevItem :: ListCursor Double -> Maybe Double)
    describe "listCursorSelectNextChar" $
        it "produces valid items" $
        producesValidsOnValids
            (listCursorNextItem :: ListCursor Double -> Maybe Double)
    describe "listCursorSelectStart" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (listCursorSelectStart :: ListCursor Double -> ListCursor Double)
        it "is a movement" $ isMovement listCursorSelectStart
        it "is idempotent" $
            idempotent
                (listCursorSelectStart :: ListCursor Double -> ListCursor Double)
    describe "listCursorSelectEnd" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (listCursorSelectEnd :: ListCursor Double -> ListCursor Double)
        it "is a movement" $ isMovement listCursorSelectEnd
        it "is idempotent" $
            idempotent
                (listCursorSelectEnd :: ListCursor Double -> ListCursor Double)
    describe "listCursorInsert" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (listCursorInsert d :: ListCursor Double -> ListCursor Double)
        it "inserts an item before the cursor" $ pending
    describe "listCursorAppend" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (listCursorAppend d :: ListCursor Double -> ListCursor Double)
        it "inserts an item after the cursor" $ pending
    describe "listCursorRemove" $ do
        it "produces valids" $
            validIfSucceedsOnValid
                (listCursorRemove :: ListCursor Double -> Maybe (ListCursor Double))
        it "removes an item before the cursor" $ pending
    describe "listCursorDelete" $ do
        it "produces valids" $
            validIfSucceedsOnValid
                (listCursorDelete :: ListCursor Double -> Maybe (ListCursor Double))
        it "removes an item before the cursor" $ pending
    describe "listCursorSplit" $ do
        it "produces valids" $
            producesValidsOnValids
                (listCursorSplit :: ListCursor Double -> ( ListCursor Double
                                                         , ListCursor Double))
        it
            "produces two list cursors that rebuild to the rebuilding of the original" $
            forAllValid $ \lc ->
                let (lc1, lc2) = listCursorSplit (lc :: ListCursor Double)
                in (rebuildListCursor lc1 ++ rebuildListCursor lc2) `shouldBe`
                   rebuildListCursor lc
    describe "listCursorCombine" $ do
        it "produces valids" $
            producesValidsOnValids2
                (listCursorCombine :: ListCursor Double -> ListCursor Double -> ListCursor Double)
        it
            "produces a list that rebuilds to the rebuilding of the original two cursors" $
            forAllValid $ \lc1 ->
                forAllValid $ \lc2 ->
                    let lc = listCursorCombine lc1 (lc2 :: ListCursor Double)
                    in rebuildListCursor lc `shouldBe`
                       (rebuildListCursor lc1 ++ rebuildListCursor lc2)

isMovementM :: (forall a. ListCursor a -> Maybe (ListCursor a)) -> Property
isMovementM func =
    forAllValid $ \lec ->
        case func (lec :: ListCursor Int) of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildListCursor lec
                    ne' = rebuildListCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ show lec
                       , "List before:  \n" ++ show ne
                       , "Cursor after: \n" ++ show lec'
                       , "List after:   \n" ++ show ne'
                       ]

isMovement :: (forall a. ListCursor a -> ListCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildListCursor (lec :: ListCursor Int) `shouldBe`
        rebuildListCursor (func lec)
