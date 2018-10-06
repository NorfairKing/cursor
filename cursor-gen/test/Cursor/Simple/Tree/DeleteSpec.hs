{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.Tree.DeleteSpec
    ( spec
    ) where

import Data.Tree

import Test.Hspec

import Test.Validity

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree
       (CTree(..), TreeCursor(..), closedForest, openForest)
import Cursor.Types

import Cursor.Simple.Tree.TestUtils

spec :: Spec
spec = do
    describe "treeCursorDeleteSubTreeAndSelectPrevious" $ do
        it "produces valids on valids" $
            producesValidsOnValids $
            treeCursorDeleteSubTreeAndSelectPrevious @Double
        it "deletes the current subtree selects the previous subtree" pending
    describe "treeCursorDeleteSubTreeAndSelectNext" $ do
        it "produces valids on valids" $
            producesValidsOnValids $
            treeCursorDeleteSubTreeAndSelectNext @Double
        it "deletes the current subtree selects the next subtree" pending
    describe "treeCursorDeleteSubTreeAndSelectAbove" $ do
        it "produces valids on valids" $
            producesValidsOnValids $
            treeCursorDeleteSubTreeAndSelectAbove @Double
        it "deletes the current subtree selects the above node" pending
    describe "treeCursorRemoveSubTree" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorRemoveSubTree @Double
        it "removes the current subtree" pending
    describe "treeCursorDeleteSubTree" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDeleteSubTree @Double
        it "deletes the current subtree" pending
    describe "treeCursorDeleteElemAndSelectPrevious" $ do
        it "produces valids on valids" $
            producesValidsOnValids $
            treeCursorDeleteElemAndSelectPrevious @Double
        it "works for this simple example" $
            forAllValid $ \fs ->
                let simpleDeleteElemStart =
                        TreeCursor
                        { treeAbove = Nothing
                        , treeCurrent = 1 :: Int
                        , treeBelow = closedForest [Node 2 fs]
                        }
                in case treeCursorDeleteElemAndSelectPrevious
                            simpleDeleteElemStart of
                       Nothing -> pure ()
                       Just Deleted ->
                           expectationFailure
                               "treeCursorDeleteElemAndSelectPrevious should not have deleted the entire example tree."
                       Just (Updated _) ->
                           expectationFailure
                               "treeCursorDeleteElemAndSelectPrevious should not have updated the example tree, but failed instead."
        it
            "deletes the current element and selects the previous element"
            pending
    describe "treeCursorDeleteElemAndSelectNext" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDeleteElemAndSelectNext @Double
        it "works for this simple example" $
            forAllValid $ \fs ->
                let simpleDeleteElemStart =
                        TreeCursor
                        { treeAbove = Nothing
                        , treeCurrent = 1
                        , treeBelow = openForest [CNode 2 fs]
                        }
                    simpleDeleteElemExpected =
                        TreeCursor
                        { treeAbove = Nothing
                        , treeCurrent = 2 :: Int
                        , treeBelow = fs
                        }
                in case treeCursorDeleteElemAndSelectNext simpleDeleteElemStart of
                       Nothing ->
                           expectationFailure
                               "treeCursorDeleteElemAndSelectNext should not have failed."
                       Just Deleted ->
                           expectationFailure
                               "treeCursorDeleteElemAndSelectNext should not have deleted the entire example tree."
                       Just (Updated f) ->
                           f `treeShouldBe` simpleDeleteElemExpected
        it "deletes the current element and selects the next element" pending
    describe "treeCursorDeleteElemAndSelectAbove" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDeleteElemAndSelectAbove @Double
        it "works for this simple example" $
            forAllValid $ \fs ->
                let simpleDeleteElemStart =
                        TreeCursor
                        { treeAbove = Nothing
                        , treeCurrent = 1 :: Int
                        , treeBelow = closedForest [Node 2 fs]
                        }
                in case treeCursorDeleteElemAndSelectAbove simpleDeleteElemStart of
                       Nothing -> pure ()
                       Just Deleted ->
                           expectationFailure
                               "treeCursorDeleteElemAndSelectAbove should not have deleted the entire example tree."
                       Just (Updated _) ->
                           expectationFailure
                               "treeCursorDeleteElemAndSelectAbove should not have updated the example tree, but failed instead."
        it "deletes the current element and selects the above element" pending
    describe "treeCursorRemoveElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorRemoveElem @Double
        it "removes the current element" pending
    describe "treeCursorDeleteElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDeleteElem @Double
        it "deletes the current element" pending
