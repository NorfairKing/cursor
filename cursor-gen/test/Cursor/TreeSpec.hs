{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.TreeSpec
    ( spec
    ) where

import Test.Hspec

import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Control.Monad (unless)

import Cursor.Tree
import Cursor.Tree.Gen ()

spec :: Spec
spec = do
    eqSpec @(TreeCursor Int)
    functorSpec @TreeCursor
    genValidSpec @(TreeCursor Double)
    describe "makeTreeCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeTreeCursor @Double)
    describe "singletonTreeCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (singletonTreeCursor @Double)
    describe "rebuildTreeCursor" $ do
        it "produces valid trees" $
            producesValidsOnValids (rebuildTreeCursor @Double)
        it "is the inverse of makeTreeCursor for integers" $
            inverseFunctions (makeTreeCursor @Int) rebuildTreeCursor
    describe "treeCursorSelectPrev" $ do
        testMovementM treeCursorSelectPrev
        it "selects the previous element" pending
        it "after treeCursorSelectNext is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid (treeCursorSelectNext @Double) $
                treeCursorSelectPrev @Double
    describe "treeCursorSelectNext" $ do
        testMovementM treeCursorSelectNext
        it "selects the next element" pending
        it "after treeCursorSelectPrev is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid (treeCursorSelectPrev @Double) $
                treeCursorSelectNext @Double
    describe "treeCursorSelectFirst" $ do
        testMovement treeCursorSelectFirst
        it "selects the first element" pending
        it "is idempotent" $ idempotentOnValid $ treeCursorSelectFirst @Double
    describe "treeCursorSelectLast" $ do
        testMovement treeCursorSelectLast
        it "selects the last element" pending
        it "is idempotent" $ idempotentOnValid $ treeCursorSelectLast @Double
    describe "treeCursorSelectAbove" $ do
        testMovementM treeCursorSelectAbove
        it "selects the element above" pending
        it "after treeCursorSelectBelow is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid (treeCursorSelectBelow @Double) $
                treeCursorSelectAbove @Double
    describe "treeCursorSelectBelow" $ do
        testMovementM treeCursorSelectBelow
        it "selects the element below" pending
    describe "treeCursorSelectBelowAtPos" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorSelectBelowAtPos @Double
        it "is a movement" $
            forAllValid $ \n -> isMovementM $ treeCursorSelectBelowAtPos n
        it "selects the element n-th below" pending
    describe "treeCursorInsert" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorInsert @Double
        it "inserts the element" pending
    describe "treeCursorInsertAndSelect" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorInsertAndSelect @Double
        it "inserts and select the element" pending
    describe "treeCursorAppend" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorAppend @Double
        it "appends the element" pending
    describe "treeCursorAppendAndSelect" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorAppendAndSelect @Double
        it "appends and select the element" pending
    describe "treeCursorRemoveElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorRemoveElem @Double
        it "after treeCursorAppendAndSelect is identity if they don't fail" $
            forAllValid $ \tree ->
                inverseFunctionsIfSucceedOnValid
                    ((treeCursorAppendAndSelect @Double) tree) $
                treeCursorRemoveElem @Double
        it "removes elements and select the former" pending
    describe "treeCursorDeleteElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDeleteElem @Double
        it "after treeCursorInsertAndSelect is identity if they don't fail" $
            forAllValid $ \tree ->
                inverseFunctionsIfSucceedOnValid
                    ((treeCursorInsertAndSelect @Double) tree) $
                (treeCursorDeleteElem @Double)
        it "removes elements and select the next" pending
    describe "treeCursorSwapPrev" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorSwapPrev @Double
        it "swaps the current node with the previous node" pending
    describe "treeCursorSwapNext" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorSwapNext @Double
        it "swaps the current node with the next node" pending
    describe "treeCursorAboveL" $
        lensSpecOnValid (treeCursorAboveL @Double)
    describe "treeCursorCurrentL" $
        lensSpecOnValid (treeCursorCurrentL @Double)
    describe "treeCursorBelowL" $
        lensSpecOnValid (treeCursorBelowL @Double)
    describe "treeAboveLeftsL" $
        lensSpecOnValid (treeAboveLeftsL @Double)
    describe "treeAboveAboveL" $
        lensSpecOnValid (treeAboveAboveL @Double)
    describe "treeAboveNodeL" $
        lensSpecOnValid (treeAboveNodeL @Double)
    describe "treeAboveRightsL" $
        lensSpecOnValid (treeAboveRightsL @Double)

testMovement :: (forall a. TreeCursor a -> TreeCursor a) -> Spec
testMovement func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovement func

testMovementM :: (forall a. TreeCursor a -> Maybe (TreeCursor a)) -> Spec
testMovementM func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovementM func

isMovementM :: (forall a. TreeCursor a -> Maybe (TreeCursor a)) -> Property
isMovementM func =
    forAllValid @(TreeCursor Int) $ \lec ->
        case func lec of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildTreeCursor lec
                    ne' = rebuildTreeCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ show lec
                       , "Tree before:  \n" ++ show ne
                       , "Cursor after: \n" ++ show lec'
                       , "Tree after:   \n" ++ show ne'
                       ]

isMovement :: (forall a. TreeCursor a -> TreeCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildTreeCursor (lec :: TreeCursor Int) `shouldBe`
        rebuildTreeCursor (func lec)
