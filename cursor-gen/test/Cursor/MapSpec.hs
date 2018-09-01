{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.MapSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Control.Monad

import Cursor.Map
import Cursor.Map.Gen ()

spec :: Spec
spec = do
    eqSpec @(MapCursor Int Int)
    functorSpec @(MapCursor Int)
    genValidSpec @(MapCursor Double Double)
    eqSpec @(KeyValueCursor Int Int)
    functorSpec @(KeyValueCursor Int)
    genValidSpec @(KeyValueCursor Double Double)
    describe "makeMapCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeMapCursor @Double @Double)
    describe "makeMapCursorWithSelection" $
        it "produces valid cursors" $
        producesValidsOnValids2 (makeMapCursorWithSelection @Double @Double)
    describe "makeKeyValueCursor" $
        it "produces valid cursors" $
        producesValidsOnValids2 (makeKeyValueCursor @Double @Double)
    describe "singletonKeyValueCursor" $
        it "produces valid cursors" $
        producesValidsOnValids2 (singletonMapCursor @Double @Double)
    describe "rebuildMapCursor" $ do
        it "produces valid Nonempty lists" $
            producesValidsOnValids (rebuildMapCursor @Double @Double)
        it "is the inverse of makeMapCursor for integers" $
            inverseFunctions (makeMapCursor @Int @Int) rebuildMapCursor
    describe "mapCursorNonEmptyCursorL" $
        lensSpecOnValid (mapCursorNonEmptyCursorL @Double @Double)
    describe "mapCursorElemL" $ lensSpecOnValid (mapCursorElemL @Double @Double)
    describe "mapCursorElemKeyL" $
        lensSpecOnValid (mapCursorElemKeyL @Double @Double)
    describe "mapCursorElemValueL" $
        lensSpecOnValid (mapCursorElemValueL @Double @Double)
    describe "mapCursorSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids (mapCursorSelectPrev @Double @Rational)
        it "is a movement" $ isMovementM mapCursorSelectPrev
        it "selects the previous element" pending
    describe "mapCursorSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids (mapCursorSelectNext @Double @Rational)
        it "is a movement" $ isMovementM mapCursorSelectNext
        it "selects the next element" pending
    describe "mapCursorSelectFirst" $ do
        it "produces valid cursors" $
            producesValidsOnValids (mapCursorSelectFirst @Double @Rational)
        it "is a movement" $ isMovement mapCursorSelectFirst
        it "is idempotent" $
            idempotentOnValid (mapCursorSelectFirst @Double @Rational)
        it "selects the first element" pending
    describe "mapCursorSelectLast" $ do
        it "produces valid cursors" $
            producesValidsOnValids (mapCursorSelectLast @Double @Rational)
        it "is a movement" $ isMovement mapCursorSelectLast
        it "is idempotent" $
            idempotentOnValid (mapCursorSelectLast @Double @Rational)
        it "selects the last element" pending
    describe "mapCursorSelection" $ do
        it "produces valid ints" $
            producesValidsOnValids (mapCursorSelection @Double @Rational)
        it "returns the index of the currently selected element" pending
    describe "mapCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (mapCursorSelectIndex @Double @Rational)
        it "is the identity function when given the current selection" $
            forAllValid $ \nec ->
                mapCursorSelectIndex (mapCursorSelection nec) nec `shouldBe`
                Just (nec :: MapCursor Double Rational)
        it "returns selects the element at the given index" pending
    describe "mapCursorInsert" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 (mapCursorInsert @Double @Rational)
        it "inserts a character before the cursor" pending
    describe "mapCursorAppend" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 (mapCursorAppend @Double @Rational)
        it "inserts a character after the cursor" pending
    describe "mapCursorInsertAndSelect" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 (mapCursorInsertAndSelect @Double @Rational)
        it "inserts a character before the cursor and selects it" pending
    describe "mapCursorAppendAndSelect" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 (mapCursorAppendAndSelect @Double @Rational)
        it "appends a character before the cursor and selects it" pending
    describe "mapCursorRemoveElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (mapCursorRemoveElem @Double @Rational)
        it "removes an element" pending
    describe "mapCursorDeleteElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (mapCursorDeleteElem @Double @Rational)
        it "deletes an element" pending

isMovementM :: (forall k v. MapCursor k v -> Maybe (MapCursor k v)) -> Property
isMovementM func =
    forAllValid $ \lec ->
        case func (lec :: MapCursor Double Double) of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildMapCursor lec
                    ne' = rebuildMapCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ show lec
                       , "Map before:  \n" ++ show ne
                       , "Cursor after: \n" ++ show lec'
                       , "Map after:   \n" ++ show ne'
                       ]

isMovement :: (forall k v. MapCursor k v -> MapCursor k v) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildMapCursor (lec :: MapCursor Int Int) `shouldBe`
        rebuildMapCursor (func lec)
