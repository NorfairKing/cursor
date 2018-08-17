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
        it "is valid" $ shouldBeValid (emptyListCursor @Int)
    describe "makeListCursor" $
        it "produces valid list cursors" $
        producesValidsOnValids (makeListCursor @Double)
    describe "makeListCursorWithSelection" $
        it "produces valid list cursors" $
        producesValidsOnValids2 (makeListCursorWithSelection @Double)
    describe "rebuildListCursor" $ do
        it "produces valid lists" $
            producesValidsOnValids (rebuildListCursor @Double)
        it "is the inverse of makeListCursor" $
            inverseFunctions (makeListCursor @Int) rebuildListCursor
        it "is the inverse of makeListCursorWithSelection for any index" $
            forAllUnchecked $ \i ->
                inverseFunctions
                    (makeListCursorWithSelection @Int i)
                    rebuildListCursor
    describe "listCursorNull" $
        it "produces valid bools" $
        producesValidsOnValids (listCursorNull @Double)
    describe "listCursorLength" $
        it "produces valid bools" $
        producesValidsOnValids (listCursorLength @Double)
    describe "listCursorIndex" $
        it "produces valid indices" $
        producesValidsOnValids (listCursorIndex @Double)
    describe "listCursorSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectPrev @Double)
        it "is a movement" $ isMovementM listCursorSelectPrev
        it "selects the previous position" pending
    describe "listCursorSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectNext @Double)
        it "is a movement" $ isMovementM listCursorSelectNext
        it "selects the next position" pending
    describe "listCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (listCursorSelectIndex @Double)
        it "is a movement" $
            forAllUnchecked $ \ix -> isMovement (listCursorSelectIndex ix)
        it "selects the position at the given index" pending
    describe "listCursorPrevItem" $ do
        it "produces valid items" $
            producesValidsOnValids (listCursorPrevItem @Double)
        it "returns the item before the position" pending
    describe "listCursorNextItem" $ do
        it "produces valid items" $
            producesValidsOnValids (listCursorNextItem @Double)
        it "returns the item after the position" pending
    describe "listCursorSelectStart" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectStart @Double)
        it "is a movement" $ isMovement listCursorSelectStart
        it "is idempotent" $ idempotentOnValid (listCursorSelectStart @Double)
        it "selects the starting position" pending
    describe "listCursorSelectEnd" $ do
        it "produces valid cursors" $
            producesValidsOnValids (listCursorSelectEnd @Double)
        it "is a movement" $ isMovement listCursorSelectEnd
        it "is idempotent" $ idempotentOnValid (listCursorSelectEnd @Double)
        it "selects the end position" pending
    describe "listCursorInsert" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids (listCursorInsert @Double d)
        it "inserts an item before the cursor" $ pending
    describe "listCursorAppend" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids (listCursorAppend @Double d)
        it "inserts an item after the cursor" $ pending
    describe "listCursorRemove" $ do
        it "produces valids" $ validIfSucceedsOnValid (listCursorRemove @Double)
        it "removes an item before the cursor" $ pending
    describe "listCursorDelete" $ do
        it "produces valids" $ validIfSucceedsOnValid (listCursorDelete @Double)
        it "removes an item before the cursor" $ pending
    describe "listCursorSplit" $ do
        it "produces valids" $ producesValidsOnValids (listCursorSplit @Double)
        it
            "produces two list cursors that rebuild to the rebuilding of the original" $
            forAllValid $ \lc ->
                let (lc1, lc2) = listCursorSplit (lc :: ListCursor Double)
                 in (rebuildListCursor lc1 ++ rebuildListCursor lc2) `shouldBe`
                    rebuildListCursor lc
    describe "listCursorCombine" $ do
        it "produces valids" $
            producesValidsOnValids2 (listCursorCombine @Double)
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
