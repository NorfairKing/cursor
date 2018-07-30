{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.ForestSpec
    ( spec
    ) where

import Test.Hspec

import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Control.Monad (unless)

import Cursor.Forest
import Cursor.Forest.Gen ()

spec :: Spec
spec = do
    eqSpec @(ForestCursor Int)
    functorSpec @ForestCursor
    genValidSpec @(ForestCursor Double)
    describe "makeForestCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeForestCursor @Double)
    describe "rebuildForestCursor" $ do
        it "produces valid forests" $
            producesValidsOnValids (rebuildForestCursor @Double)
        it "is the inverse of makeForestCursor for integers" $
            inverseFunctions (makeForestCursor @Int) rebuildForestCursor
    describe "forestCursorListCursorL" $
        lensSpecOnValid (forestCursorListCursorL @Double)
    describe "forestCursorSelectedTreeL" $
        lensSpecOnValid (forestCursorSelectedTreeL @Double)
    describe "forestCursorSelectPrevTree" $ do
        testMovementM forestCursorSelectPrevTree
        it "selects the previous tree" pending
    describe "forestCursorSelectNextTree" $ do
        testMovementM forestCursorSelectNextTree
        it "selects the next tree" pending
    describe "forestCursorSelectFirstTree" $ do
        testMovement forestCursorSelectFirstTree
        it "selects the first tree" pending
    describe "forestCursorSelectLastTree" $ do
        testMovement forestCursorSelectLastTree
        it "selects the last tree" pending
    describe "forestCursorSelection" $ do
        it "produces valid ints" $
            producesValidsOnValids (forestCursorSelection @Double)
        it "returns the index of the currently selected element" pending
    describe "forestCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorSelectIndex @Double)
        it "is the identity function when given the current selection" $
            forAllValid $ \fc ->
                forestCursorSelectIndex fc (forestCursorSelection fc) `shouldBe`
                Just (fc :: ForestCursor Double)
        it "returns selects the element at the given index" pending
    describe "forestCursorInsert" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsert @Double)
        it "inserts a tree before the currently selected tree" pending
    describe "forestCursorAppend" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppend @Double)
        it "appends a tree after the currently selected tree" pending
    describe "forestCursorInsertAndSelect" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsertAndSelect @Double)
        it
            "inserts a tree before the currently selected tree and selects it"
            pending
    describe "forestCursorAppendAndSelect" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppendAndSelect @Double)
        it
            "appends a tree after the currently selected tree and selects it"
            pending
    describe "forestCursorRemoveTreeAndSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorRemoveTreeAndSelectPrev @Double)
        it "removes the selected tree and selects the previous tree" pending
    describe "forestCursorDeleteTreeAndSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorDeleteTreeAndSelectNext @Double)
        it "deletes the selected tree and selects the next tree" pending
    describe "forestCursorRemoveTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorRemoveTree @Double)
        it "removes the selected tree" pending
    describe "forestCursorDeleteTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorDeleteTree @Double)
        it "deletes the selected tree" pending
    describe "forestCursorAddRoot" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAddRoot @Double)
        it "houses the entire forest under the given node" pending

testMovement :: (forall a. ForestCursor a -> ForestCursor a) -> Spec
testMovement func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovement func

testMovementM :: (forall a. ForestCursor a -> Maybe (ForestCursor a)) -> Spec
testMovementM func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovementM func

isMovementM :: (forall a. ForestCursor a -> Maybe (ForestCursor a)) -> Property
isMovementM func =
    forAllValid @(ForestCursor Int) $ \lec ->
        case func lec of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildForestCursor lec
                    ne' = rebuildForestCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ show lec
                       , "Forest before:  \n" ++ show ne
                       , "Cursor after: \n" ++ show lec'
                       , "Forest after:   \n" ++ show ne'
                       ]

isMovement :: (forall a. ForestCursor a -> ForestCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildForestCursor (lec :: ForestCursor Int) `shouldBe`
        rebuildForestCursor (func lec)
