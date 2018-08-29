{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.TreeSpec
    ( spec
    ) where

import Data.Tree

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
            inverseFunctionsIfSucceedOnValid
                (treeCursorSelectNext @Double)
                (treeCursorSelectPrev @Double)
    describe "treeCursorSelectNext" $ do
        testMovementM treeCursorSelectNext
        it "selects the next element" pending
        it "after treeCursorSelectPrev is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid
                (treeCursorSelectPrev @Double)
                (treeCursorSelectNext @Double)
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
            inverseFunctionsIfSucceedOnValid
                (treeCursorSelectBelowAtStart @Double) $
                treeCursorSelectAbove @Double
    describe "treeCursorSelectBelowAtPos" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorSelectBelowAtPos @Double
        it "is a movement" $
            forAllValid $ \n -> isMovementM $ treeCursorSelectBelowAtPos n
        it "selects the element n-th below" pending
    describe "treeCursorSelectBelowAtStart" $ do
        testMovementM treeCursorSelectBelowAtStart
        it "selects the first child below" pending
    describe "treeCursorSelectBelowAtEnd" $ do
        testMovementM treeCursorSelectBelowAtEnd
        it "selects the last child below" pending
    describe "treeCursorSelectPrevOnSameLevel" $ do
        testMovementM treeCursorSelectPrevOnSameLevel
        it "selects the previous element" pending
        it
            "after treeCursorSelectNextOnSameLevel is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid
                (treeCursorSelectNextOnSameLevel @Double)
                (treeCursorSelectPrevOnSameLevel @Double)
    describe "treeCursorSelectNextOnSameLevel" $ do
        testMovementM treeCursorSelectNextOnSameLevel
        it "selects the next element" pending
        it
            "after treeCursorSelectPrevOnSameLevel is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid
                (treeCursorSelectPrevOnSameLevel @Double)
                (treeCursorSelectNextOnSameLevel @Double)
    describe "treeCursorSelectAbovePrev" $ do
        testMovementM treeCursorSelectAbovePrev
        it "Works for this classic example" $
            -- > 0
            --   > 1
            --     > 2
            --       > 3 <- expected end cursor
            --   > 4 <- start cursor
         do
            let start =
                    TreeCursor
                    { treeAbove =
                          Just
                              (TreeAbove
                               { treeAboveLefts = [Node 1 [Node 2 [Node 3 []]]]
                               , treeAboveAbove = Nothing
                               , treeAboveNode = 0
                               , treeAboveRights = []
                               })
                    , treeCurrent = 4 :: Int
                    , treeBelow = []
                    }
                expected =
                    TreeCursor
                    { treeAbove =
                          Just
                              (TreeAbove
                               { treeAboveLefts = []
                               , treeAboveAbove =
                                     Just
                                         (TreeAbove
                                          { treeAboveLefts = []
                                          , treeAboveAbove =
                                                Just
                                                    (TreeAbove
                                                     { treeAboveLefts = []
                                                     , treeAboveAbove = Nothing
                                                     , treeAboveNode = 0
                                                     , treeAboveRights =
                                                           [Node 4 []]
                                                     })
                                          , treeAboveNode = 1
                                          , treeAboveRights = []
                                          })
                               , treeAboveNode = 2
                               , treeAboveRights = []
                               })
                    , treeCurrent = 3
                    , treeBelow = []
                    }
            treeCursorSelectAbovePrev start `shouldBe` Just expected
        it "selects the previous element" pending
        it "after treeCursorSelectAboveNext is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid
                (treeCursorSelectAboveNext @Double)
                (treeCursorSelectAbovePrev @Double)
    describe "treeCursorSelectAboveNext" $ do
        testMovementM treeCursorSelectAboveNext
        it "Works for this classic example" $
            -- > 0
            --   > 1
            --     > 2
            --       > 3 <- start cursor
            --   > 4 <- expected end cursor
         do
            let start =
                    TreeCursor
                    { treeAbove =
                          Just
                              (TreeAbove
                               { treeAboveLefts = []
                               , treeAboveAbove =
                                     Just
                                         (TreeAbove
                                          { treeAboveLefts = []
                                          , treeAboveAbove =
                                                Just
                                                    (TreeAbove
                                                     { treeAboveLefts = []
                                                     , treeAboveAbove = Nothing
                                                     , treeAboveNode = 0
                                                     , treeAboveRights =
                                                           [Node 4 []]
                                                     })
                                          , treeAboveNode = 1
                                          , treeAboveRights = []
                                          })
                               , treeAboveNode = 2
                               , treeAboveRights = []
                               })
                    , treeCurrent = 3
                    , treeBelow = []
                    }
                expected =
                    TreeCursor
                    { treeAbove =
                          Just
                              (TreeAbove
                               { treeAboveLefts = [Node 1 [Node 2 [Node 3 []]]]
                               , treeAboveAbove = Nothing
                               , treeAboveNode = 0
                               , treeAboveRights = []
                               })
                    , treeCurrent = 4 :: Int
                    , treeBelow = []
                    }
            treeCursorSelectAboveNext start `shouldBe` Just expected
        it "selects the next element" pending
        it "after treeCursorSelectAbovePrev is identity if they don't fail" $ do
            inverseFunctionsIfSucceedOnValid
                (treeCursorSelectAbovePrev @Double)
                (treeCursorSelectAboveNext @Double)
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
    describe "treeCursorAddChildAtPos" $ do
        it "produces valid cursors " $
            producesValidsOnValids3 $ treeCursorAddChildAtPos @Double
        it
            "adds a tree at the given index in the children of the current node"
            pending
    describe "treeCursorAddChildAtStart" $ do
        it "produces valid cursors " $
            producesValidsOnValids2 $ treeCursorAddChildAtStart @Double
        it
            "adds a tree at the start of the children of the current node"
            pending
    describe "treeCursorAddChildAtEnd" $ do
        it "produces valid cursors " $
            producesValidsOnValids2 $ treeCursorAddChildAtEnd @Double
        it "adds a tree at the end of the children of the current node" pending
    describe "treeCursorDeleteElemAndSelectPrevious" $ do
        it "produces valids on valids" $
            producesValidsOnValids $
            treeCursorDeleteElemAndSelectPrevious @Double
        it
            "deletes the current node and children and selects the previous tree"
            pending
    describe "treeCursorDeleteElemAndSelectNext" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDeleteElemAndSelectNext @Double
        it
            "deletes the current node and children and selects the next tree"
            pending
    describe "treeCursorRemoveElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorRemoveElem @Double
        it "removes elements and select the former" pending
    describe "treeCursorDeleteElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDeleteElem @Double
        it "removes elements and select the next" pending
    describe "treeCursorSwapPrev" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorSwapPrev @Double
        it "swaps the current node with the previous node" pending
    describe "treeCursorSwapNext" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorSwapNext @Double
        it "swaps the current node with the next node" pending
    describe "treeCursorAboveL" $ lensSpecOnValid (treeCursorAboveL @Double)
    describe "treeCursorCurrentL" $ lensSpecOnValid (treeCursorCurrentL @Double)
    describe "treeCursorBelowL" $ lensSpecOnValid (treeCursorBelowL @Double)
    describe "treeAboveLeftsL" $ lensSpecOnValid (treeAboveLeftsL @Double)
    describe "treeAboveAboveL" $ lensSpecOnValid (treeAboveAboveL @Double)
    describe "treeAboveNodeL" $ lensSpecOnValid (treeAboveNodeL @Double)
    describe "treeAboveRightsL" $ lensSpecOnValid (treeAboveRightsL @Double)

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
