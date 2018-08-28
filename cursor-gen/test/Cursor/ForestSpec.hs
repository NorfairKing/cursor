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
    describe "forestCursorInsertTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsertTreeCursor @Double)
        it
            "inserts a tree cursor before the currently selected tree cursor"
            pending
    describe "forestCursorInsertAndSelectTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids2
                (forestCursorInsertAndSelectTreeCursor @Double)
        it
            "inserts a tree cursor before the currently selected tree cursor and selects it"
            pending
    describe "forestCursorAppendTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppendTreeCursor @Double)
        it "appends a tree after the currently selected tree cursor" pending
    describe "forestCursorAppendAndSelectTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids2
                (forestCursorAppendAndSelectTreeCursor @Double)
        it
            "appends a tree cursor after the currently selected tree cursor and selects it"
            pending
    describe "forestCursorInsertTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsertTree @Double)
        it "inserts a tree before the currently selected tree" pending
    describe "forestCursorInsertAndSelectTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsertAndSelectTree @Double)
        it
            "inserts a tree before the currently selected tree and selects it"
            pending
    describe "forestCursorAppendTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppendTree @Double)
        it "appends a tree after the currently selected tree " pending
    describe "forestCursorAppendAndSelectTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppendAndSelectTree @Double)
        it
            "appends a tree after the currently selected tree and selects it"
            pending
    describe "forestCursorInsert" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsert @Double)
        it "inserts a node before the currently selected node" pending
    describe "forestCursorInsertAndSelect" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsertAndSelect @Double)
        it
            "inserts a node before the currently selected node and selects it"
            pending
    describe "forestCursorAppend" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppend @Double)
        it "appends a node after the currently selected node" pending
    describe "forestCursorAppendAndSelect" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppendAndSelect @Double)
        it
            "appends a node after the currently selected node and selects it"
            pending
    describe "forestCursorAddChildToNodeAtPos" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 $ forestCursorAddChildToNodeAtPos @Double
        it
            "adds a child to a node at the given position in the children of that node"
            pending
    describe "forestCursorAddChildToNodeAtStart" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 $ forestCursorAddChildToNodeAtStart @Double
        it
            "adds a child to a node at the start the children of that node"
            pending
    describe "forestCursorAddChildToNodeAtEnd" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 $ forestCursorAddChildToNodeAtEnd @Double
        it "adds a child to a node at the end the children of that node" pending
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
