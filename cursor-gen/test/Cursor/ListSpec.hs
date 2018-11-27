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
    genValidSpec @(ListCursor Rational)
    describe "emptyListCursor" $
        it "is valid" $ shouldBeValid (emptyListCursor @Int)
    describe "makeListCursor" $
        it "produces valid list cursors" $
        producesValidsOnValids (makeListCursor @Rational)
    describe "makeListCursorWithSelection" $
        it "produces valid list cursors" $
        producesValidsOnValids2 (makeListCursorWithSelection @Rational)
    describe "rebuildListCursor" $ do
        it "produces valid lists" $
            producesValidsOnValids (rebuildListCursor @Rational)
        it "is the inverse of makeListCursor" $
            inverseFunctions (makeListCursor @Int) rebuildListCursor
        it "is the inverse of makeListCursorWithSelection for any index" $
            forAllUnchecked $ \i ->
                inverseFunctionsIfFirstSucceeds
                    (makeListCursorWithSelection @Int i)
                    rebuildListCursor
    describe "listCursorNull" $
        it "produces valid bools" $
        producesValidsOnValids (listCursorNull @Rational)
    describe "listCursorLength" $
        it "produces valid bools" $
        producesValidsOnValids (listCursorLength @Rational)
    describe "listCursorIndex" $
        it "produces valid indices" $
        producesValidsOnValids (listCursorIndex @Rational)
    describe "listCursorSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectPrev @Rational)
        it "is a movement" $ isMovementM listCursorSelectPrev
        it "is the inverse of listCursorSelectNext" $
            inverseFunctionsIfSucceed
                (listCursorSelectNext @Rational)
                listCursorSelectPrev
        it "selects the previous position" pending
    describe "listCursorSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectNext @Rational)
        it "is a movement" $ isMovementM listCursorSelectNext

        it "is the inverse of listCursorSelectPrev" $
            inverseFunctionsIfSucceed
                (listCursorSelectPrev @Rational)
                listCursorSelectNext
        it "selects the next position" pending
    describe "listCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (listCursorSelectIndex @Rational)
        it "is a movement" $
            forAllUnchecked $ \ix -> isMovement (listCursorSelectIndex ix)
        it "selects the position at the given index" pending
    describe "listCursorPrevItem" $ do
        it "produces valid items" $
            producesValidsOnValids (listCursorPrevItem @Rational)
        it "returns the item before the position" pending
    describe "listCursorNextItem" $ do
        it "produces valid items" $
            producesValidsOnValids (listCursorNextItem @Rational)
        it "returns the item after the position" pending
    describe "listCursorSelectStart" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectStart @Rational)
        it "is a movement" $ isMovement listCursorSelectStart
        it "is idempotent" $ idempotentOnValid (listCursorSelectStart @Rational)
        it "selects the starting position" pending
    describe "listCursorSelectEnd" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectEnd @Rational)
        it "is a movement" $ isMovement listCursorSelectEnd
        it "is idempotent" $ idempotentOnValid (listCursorSelectEnd @Rational)
        it "selects the end position" pending
    describe "listCursorInsert" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids (listCursorInsert @Rational d)
        it "inserts an item before the cursor" $ pending
    describe "listCursorAppend" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids (listCursorAppend @Rational d)
        it "inserts an item after the cursor" $ pending
    describe "listCursorRemove" $ do
        it "produces valids" $ validIfSucceedsOnValid (listCursorRemove @Rational)
        it "removes an item before the cursor" $ pending
    describe "listCursorDelete" $ do
        it "produces valids" $ validIfSucceedsOnValid (listCursorDelete @Rational)
        it "removes an item before the cursor" $ pending
    describe "listCursorSplit" $ do
        it "produces valids" $ producesValidsOnValids (listCursorSplit @Rational)
        it
            "produces two list cursors that rebuild to the rebuilding of the original" $
            forAllValid $ \lc ->
                let (lc1, lc2) = listCursorSplit (lc :: ListCursor Rational)
                 in (rebuildListCursor lc1 ++ rebuildListCursor lc2) `shouldBe`
                    rebuildListCursor lc
    describe "listCursorCombine" $ do
        it "produces valids" $
            producesValidsOnValids2 (listCursorCombine @Rational)
        it
            "produces a list that rebuilds to the rebuilding of the original two cursors" $
            forAllValid $ \lc1 ->
                forAllValid $ \lc2 ->
                    let lc = listCursorCombine lc1 (lc2 :: ListCursor Rational)
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
