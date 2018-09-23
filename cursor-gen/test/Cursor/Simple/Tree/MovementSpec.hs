{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.Tree.MovementSpec
    ( spec
    ) where

import Control.Monad (unless)

import Text.Show.Pretty

import Test.Hspec

import Test.Validity

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree
       (CTree(..), TreeAbove(..), TreeCursor(..), emptyCForest,
        openForest)

import Cursor.Simple.Tree.TestUtils

spec :: Spec
spec = do
    describe "treeCursorSelection" $
        it "produces valids on valids" $
        producesValidsOnValids (treeCursorSelection @Double @Double)
    describe "treeCursorSelect" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 (treeCursorSelect @Double)
        it "is identity with the current selection" $
            forAllValid $ \tc ->
                let sel = treeCursorSelection tc
                in case treeCursorSelect @Double sel tc of
                       Nothing ->
                           expectationFailure
                               "treeCursorSelect should not have failed."
                       Just r ->
                           unless (r == tc) $
                           expectationFailure $
                           unlines
                               [ "selection:"
                               , ppShow sel
                               , "expected:"
                               , drawTreeCursor tc
                               , "actual:"
                               , drawTreeCursor r
                               ]
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
                               { treeAboveLefts =
                                     [ CNode 1 $
                                       openForest
                                           [ CNode 2 $
                                             openForest [CNode 3 emptyCForest]
                                           ]
                                     ]
                               , treeAboveAbove = Nothing
                               , treeAboveNode = 0
                               , treeAboveRights = []
                               })
                    , treeCurrent = 4 :: Int
                    , treeBelow = emptyCForest
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
                                                           [ CNode
                                                                 4
                                                                 emptyCForest
                                                           ]
                                                     })
                                          , treeAboveNode = 1
                                          , treeAboveRights = []
                                          })
                               , treeAboveNode = 2
                               , treeAboveRights = []
                               })
                    , treeCurrent = 3
                    , treeBelow = emptyCForest
                    }
            case treeCursorSelectAbovePrev start of
                Nothing ->
                    expectationFailure
                        "treeCursorSelectAbovePrev should not have failed"
                Just r -> r `treeShouldBe` expected
        it "selects the previous element" pending
        it "after treeCursorSelectAboveNext is identity if they don't fail" $ do
            forAllValid $ \tc ->
                case treeCursorSelectAboveNext @Double tc of
                    Nothing -> pure ()
                    Just tc' ->
                        case treeCursorSelectAbovePrev tc' of
                            Nothing ->
                                expectationFailure
                                    "treeCursorSelectAbovePrev should not have failed."
                            Just tc'' ->
                                unless (tc == tc'') $
                                expectationFailure $
                                unlines
                                    [ "treeCursorSelectAboveNext and treeCursorSelectAbovePrev should have round-tripped."
                                    , "Started with:"
                                    , drawTreeCursor tc
                                    , "after treeCursorSelectAboveNext"
                                    , drawTreeCursor tc'
                                    , "after treeCursorSelectAbovePrev"
                                    , drawTreeCursor tc''
                                    , "instead of"
                                    , drawTreeCursor tc
                                    ]
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
                                                           [node 4 []]
                                                     })
                                          , treeAboveNode = 1
                                          , treeAboveRights = []
                                          })
                               , treeAboveNode = 2
                               , treeAboveRights = []
                               })
                    , treeCurrent = 3
                    , treeBelow = emptyCForest
                    }
                expected =
                    TreeCursor
                    { treeAbove =
                          Just
                              (TreeAbove
                               { treeAboveLefts =
                                     [ CNode 1 $
                                       openForest
                                           [ CNode 2 $
                                             openForest [CNode 3 emptyCForest]
                                           ]
                                     ]
                               , treeAboveAbove = Nothing
                               , treeAboveNode = 0
                               , treeAboveRights = []
                               })
                    , treeCurrent = 4 :: Int
                    , treeBelow = emptyCForest
                    }
            case treeCursorSelectAboveNext start of
                Nothing ->
                    expectationFailure
                        "treeCursorSelectAboveNext should not have failed."
                Just r -> r `treeShouldBe` expected
        it "selects the next element" pending
        it "after treeCursorSelectAbovePrev is identity if they don't fail" $ do
            forAllValid $ \tc ->
                case treeCursorSelectAbovePrev @Double tc of
                    Nothing -> pure ()
                    Just tc' ->
                        case treeCursorSelectAboveNext tc' of
                            Nothing -> pure ()
                            Just tc'' ->
                                unless (tc == tc'') $
                                expectationFailure $
                                unlines
                                    [ "treeCursorSelectAbovePrev and treeCursorSelectAboveNext should have round-tripped."
                                    , "Started with:"
                                    , drawTreeCursor tc
                                    , "after treeCursorSelectAbovePrev"
                                    , drawTreeCursor tc'
                                    , "after treeCursorSelectAboveNext"
                                    , drawTreeCursor tc''
                                    , "instead of"
                                    , drawTreeCursor tc
                                    ]
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
    describe "treeCursorSelectBelowAtStartRecursively" $ do
        testMovementM treeCursorSelectBelowAtStartRecursively
        it "selects the first child below, recursively" pending
    describe "treeCursorSelectBelowAtEndRecursively" $ do
        testMovementM treeCursorSelectBelowAtEndRecursively
        it "selects the last child below, recursively" pending
