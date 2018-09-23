{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Simple.TreeSpec
    ( spec
    ) where

import Data.Tree

import Control.Monad (unless)

import Text.Show.Pretty

import Test.Hspec

import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Cursor.Simple.Tree hiding (TreeCursor)
import qualified Cursor.Simple.Tree as STC (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree (TreeAbove(..), TreeCursor(..))
import Cursor.Types

spec :: Spec
spec = do
    eqSpec @(STC.TreeCursor Int)
    genValidSpec @(STC.TreeCursor Double)
    describe "makeTreeCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeTreeCursor @Double)
    describe "makeTreeCursorWithSelection" $
        it "produces valid cursors" $
        producesValidsOnValids2 (makeTreeCursorWithSelection @Double)
    describe "singletonTreeCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (singletonTreeCursor @Double)
    describe "rebuildTreeCursor" $ do
        it "produces valid trees" $
            producesValidsOnValids (rebuildTreeCursor @Double)
        it "is the inverse of makeTreeCursor for integers" $
            inverseFunctions (makeTreeCursor @Int) rebuildTreeCursor
        it
            "is the inverse of makeTreeCursorWithSelection for the current selection" $
            forAllValid $ \tc ->
                case makeTreeCursorWithSelection
                         @Double
                         (treeCursorSelection tc)
                         (rebuildTreeCursor tc) of
                    Nothing ->
                        expectationFailure
                            "makeTreeCursorWithSelection should not have failed."
                    Just r -> r `treeShouldBe` tc
    describe "treeCursorAboveL" $
        lensSpecOnValid (treeCursorAboveL @Double @Double)
    describe "treeCursorCurrentL" $
        lensSpecOnValid (treeCursorCurrentL @Double @Double)
    describe "treeCursorBelowL" $
        lensSpecOnValid (treeCursorBelowL @Double @Double)
    describe "treeAboveLeftsL" $ lensSpecOnValid (treeAboveLeftsL @Double)
    describe "treeAboveAboveL" $ lensSpecOnValid (treeAboveAboveL @Double)
    describe "treeAboveNodeL" $ lensSpecOnValid (treeAboveNodeL @Double)
    describe "treeAboveRightsL" $ lensSpecOnValid (treeAboveRightsL @Double)
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
                               { treeAboveLefts = [node 1 [node 2 [node 3 []]]]
                               , treeAboveAbove = Nothing
                               , treeAboveNode = 0
                               , treeAboveRights = []
                               })
                    , treeCurrent = 4 :: Int
                    , treeBelow = makeCollapse []
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
                                                           [node 4 []]
                                                     })
                                          , treeAboveNode = 1
                                          , treeAboveRights = []
                                          })
                               , treeAboveNode = 2
                               , treeAboveRights = []
                               })
                    , treeCurrent = 3
                    , treeBelow = makeCollapse []
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
                    , treeBelow = makeCollapse []
                    }
                expected =
                    TreeCursor
                    { treeAbove =
                          Just
                              (TreeAbove
                               { treeAboveLefts = [node 1 [node 2 [node 3 []]]]
                               , treeAboveAbove = Nothing
                               , treeAboveNode = 0
                               , treeAboveRights = []
                               })
                    , treeCurrent = 4 :: Int
                    , treeBelow = makeCollapse []
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
    describe "treeCursorInsert" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorInsert @Double @Double
        it "inserts the element" pending
    describe "treeCursorInsertAndSelect" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorInsertAndSelect @Double
        it "inserts and select the element" pending
    describe "treeCursorAppend" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorAppend @Double @Double
        it "appends the element" pending
    describe "treeCursorAppendAndSelect" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $ treeCursorAppendAndSelect @Double
        it "appends and select the element" pending
    describe "treeCursorAddChildAtPos" $ do
        it "produces valid cursors " $
            producesValidsOnValids3 $ treeCursorAddChildAtPos @Double @Double
        it
            "adds a tree at the given index in the children of the current node"
            pending
    describe "treeCursorAddChildAtStart" $ do
        it "produces valid cursors " $
            producesValidsOnValids2 $ treeCursorAddChildAtStart @Double @Double
        it
            "adds a tree at the start of the children of the current node"
            pending
    describe "treeCursorAddChildAtEnd" $ do
        it "produces valid cursors " $
            producesValidsOnValids2 $ treeCursorAddChildAtEnd @Double @Double
        it "adds a tree at the end of the children of the current node" pending
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
                        , treeBelow = makeCollapse [node 2 fs]
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
                        , treeBelow = makeCollapse [node 2 fs]
                        }
                    simpleDeleteElemExpected =
                        TreeCursor
                        { treeAbove = Nothing
                        , treeCurrent = 2 :: Int
                        , treeBelow = makeCollapse fs
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
                        , treeBelow = makeCollapse [node 2 fs]
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
    functorSpec @SwapResult
    describe "treeCursorSwapPrev" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorSwapPrev @Double @Double
        it "works on the example from the docs" $
            let start =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'a' []]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = []
                              }
                    , treeCurrent = 'b'
                    , treeBelow = makeCollapse []
                    }
                end =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = []
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [node 'a' []]
                              }
                    , treeCurrent = 'b'
                    , treeBelow = makeCollapse []
                    }
            in case treeCursorSwapPrev start of
                   Swapped r -> r `treeShouldBe` end
                   _ ->
                       expectationFailure
                           "treeCursorSwapPrev should not have failed."
        it "reverts treeCursorSwapNext" $
            inverseFunctionsIfSucceedOnValid
                (treeCursorSwapNext @Double @Double)
                (treeCursorSwapPrev @Double @Double)
        it "swaps the current node with the previous node" pending
    describe "treeCursorSwapNext" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorSwapNext @Double @Double
        it "works on the example from the docs" $
            let start =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = []
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [node 'b' []]
                              }
                    , treeCurrent = 'a'
                    , treeBelow = makeCollapse []
                    }
                end =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'b' []]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = []
                              }
                    , treeCurrent = 'a'
                    , treeBelow = makeCollapse []
                    }
            in case treeCursorSwapNext start of
                   Swapped r -> r `treeShouldBe` end
                   _ ->
                       expectationFailure
                           "treeCursorSwapNext should not have failed."
        it "reverts treeCursorSwapNext" $
            inverseFunctionsIfSucceedOnValid
                (treeCursorSwapPrev @Double @Double)
                (treeCursorSwapNext @Double @Double)
        it "swaps the current node with the next node" pending
    functorSpec @PromoteElemResult
    applicativeSpec @PromoteElemResult
    monadSpec @PromoteElemResult
    describe "treeCursorPromoteElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorPromoteElem @Double
        it "Works on the example from the docs" $
            let promoteStart =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'b' [node 'c' []]]
                              , treeAboveAbove =
                                    Just
                                        TreeAbove
                                        { treeAboveLefts = []
                                        , treeAboveAbove = Nothing
                                        , treeAboveNode = 'p'
                                        , treeAboveRights = [node 'h' []]
                                        }
                              , treeAboveNode = 'a'
                              , treeAboveRights = [node 'f' [node 'g' []]]
                              }
                    , treeCurrent = 'd'
                    , treeBelow = makeCollapse [node 'e' []]
                    }
                promoteEnd =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts =
                                    [ node
                                          'a'
                                          [ node 'b' [node 'c' [], node 'e' []]
                                          , node 'f' [node 'g' []]
                                          ]
                                    ]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [node 'h' []]
                              }
                    , treeCurrent = 'd'
                    , treeBelow = makeCollapse []
                    }
            in case treeCursorPromoteElem promoteStart of
                   PromotedElem tc' -> tc' `treeShouldBe` promoteEnd
                   _ ->
                       expectationFailure
                           "treeCursorPromoteElem should not have failed"
        it "promotes the current node to the level of its parent" pending
    functorSpec @PromoteResult
    applicativeSpec @PromoteResult
    monadSpec @PromoteResult
    describe "treeCursorPromoteSubTree" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorPromoteSubTree @Double
        it "Works on the example from the docs" $
            let promoteStart =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'b' [node 'c' []]]
                              , treeAboveAbove =
                                    Just
                                        TreeAbove
                                        { treeAboveLefts = []
                                        , treeAboveAbove = Nothing
                                        , treeAboveNode = 'p'
                                        , treeAboveRights = [node 'h' []]
                                        }
                              , treeAboveNode = 'a'
                              , treeAboveRights = [node 'f' [node 'g' []]]
                              }
                    , treeCurrent = 'd'
                    , treeBelow = makeCollapse [node 'e' []]
                    }
                promoteEnd =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts =
                                    [ node
                                          'a'
                                          [ node 'b' [node 'c' []]
                                          , node 'f' [node 'g' []]
                                          ]
                                    ]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [node 'h' []]
                              }
                    , treeCurrent = 'd'
                    , treeBelow = makeCollapse [node 'e' []]
                    }
            in case treeCursorPromoteSubTree promoteStart of
                   Promoted tc' -> tc' `treeShouldBe` promoteEnd
                   _ ->
                       expectationFailure
                           "treeCursorPromoteSubTree should not have failed"
        it "promotes the current subtree to the level of its parent" pending
    functorSpec @DemoteResult
    describe "treeCursorDemoteElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDemoteElem @Double
        it "Works on the example from the docs" $
            let promoteStart =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'a' [node 'b' []]]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [node 'e' []]
                              }
                    , treeCurrent = 'c'
                    , treeBelow = makeCollapse [node 'd' []]
                    }
                promoteEnd =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'b' []]
                              , treeAboveAbove =
                                    Just
                                        TreeAbove
                                        { treeAboveLefts = []
                                        , treeAboveAbove = Nothing
                                        , treeAboveNode = 'p'
                                        , treeAboveRights = [node 'e' []]
                                        }
                              , treeAboveNode = 'a'
                              , treeAboveRights = [node 'd' []]
                              }
                    , treeCurrent = 'c'
                    , treeBelow = makeCollapse []
                    }
            in case treeCursorDemoteElem promoteStart of
                   Demoted tc' -> tc' `treeShouldBe` promoteEnd
                   _ ->
                       expectationFailure
                           "treeCursorDemoteElem should not have failed"
        it "demotes the current node to the level of its children" pending
    describe "treeCursorDemoteSubTree" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ treeCursorDemoteSubTree @Double
        it "Works on the example from the docs" $
            let promoteStart =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'a' [node 'b' []]]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [node 'e' []]
                              }
                    , treeCurrent = 'c'
                    , treeBelow = makeCollapse [node 'd' []]
                    }
                promoteEnd =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts = [node 'b' []]
                              , treeAboveAbove =
                                    Just
                                        TreeAbove
                                        { treeAboveLefts = []
                                        , treeAboveAbove = Nothing
                                        , treeAboveNode = 'p'
                                        , treeAboveRights = [node 'e' []]
                                        }
                              , treeAboveNode = 'a'
                              , treeAboveRights = []
                              }
                    , treeCurrent = 'c'
                    , treeBelow = makeCollapse [node 'd' []]
                    }
            in case treeCursorDemoteSubTree promoteStart of
                   Demoted tc' -> tc' `treeShouldBe` promoteEnd
                   _ ->
                       expectationFailure
                           "treeCursorDemoteSubTree should not have failed"
        it "demotes the current subtree to the level of its children" pending
    describe "treeCursorDemoteElemUnder" $ do
        it "produces valids on valids" $
            producesValidsOnValids3 $ treeCursorDemoteElemUnder @Double @Double
        it "Works on the example from the docs" $
            forAllValid $ \b1 ->
                forAllValid $ \b2 ->
                    let demoteStart =
                            TreeCursor
                            { treeAbove =
                                  Just
                                      TreeAbove
                                      { treeAboveLefts = []
                                      , treeAboveAbove = Nothing
                                      , treeAboveNode = 'p'
                                      , treeAboveRights = []
                                      }
                            , treeCurrent = 'a'
                            , treeBelow = makeCollapse [node 'b' []]
                            }
                        demoteEnd =
                            TreeCursor
                            { treeAbove =
                                  Just
                                      TreeAbove
                                      { treeAboveLefts = []
                                      , treeAboveAbove =
                                            Just
                                                TreeAbove
                                                { treeAboveLefts = []
                                                , treeAboveAbove = Nothing
                                                , treeAboveNode = 'p'
                                                , treeAboveRights =
                                                      [node b2 [node 'b' []]]
                                                }
                                      , treeAboveNode = b1
                                      , treeAboveRights = []
                                      }
                            , treeCurrent = 'a'
                            , treeBelow = makeCollapse []
                            }
                    in case treeCursorDemoteElemUnder b1 b2 demoteStart of
                           Just tc' -> tc' `treeShouldBe` demoteEnd
                           _ ->
                               expectationFailure
                                   "treeCursorDemoteElemUnder should not have failed"
        it "demotes the current node to the level of its children" pending
    describe "treeCursorDemoteSubTreeUnder" $ do
        it "produces valids on valids" $
            producesValidsOnValids2 $
            treeCursorDemoteSubTreeUnder @Double @Double
        it "Works on the example from the docs" $
            forAllValid $ \v -> do
                let demoteStart =
                        TreeCursor
                        { treeAbove = Nothing
                        , treeCurrent = 'a'
                        , treeBelow = makeCollapse [node 'b' []]
                        }
                    demoteEnd =
                        TreeCursor
                        { treeAbove =
                              Just
                                  TreeAbove
                                  { treeAboveLefts = []
                                  , treeAboveAbove = Nothing
                                  , treeAboveNode = v
                                  , treeAboveRights = []
                                  }
                        , treeCurrent = 'a'
                        , treeBelow = makeCollapse [node 'b' []]
                        }
                treeCursorDemoteSubTreeUnder v demoteStart `treeShouldBe`
                    demoteEnd
        it
            "demotes the current subtree to the level of its children, by adding a root"
            pending

testMovement :: (forall a. STC.TreeCursor a -> STC.TreeCursor a) -> Spec
testMovement func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovement func

testMovementM ::
       (forall a. STC.TreeCursor a -> Maybe (STC.TreeCursor a)) -> Spec
testMovementM func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovementM func

isMovementM ::
       (forall a. STC.TreeCursor a -> Maybe (STC.TreeCursor a)) -> Property
isMovementM func =
    forAllValid @(STC.TreeCursor Int) $ \lec ->
        case func lec of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildCTree $ rebuildTreeCursor lec
                    ne' = rebuildCTree $ rebuildTreeCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ drawTreeCursor lec
                       , "Tree before:  \n" ++ drawTree (fmap show ne)
                       , "Cursor after: \n" ++ drawTreeCursor lec'
                       , "Tree after:   \n" ++ drawTree (fmap show ne')
                       ]

isMovement :: (forall a. STC.TreeCursor a -> STC.TreeCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildTreeCursor (lec :: STC.TreeCursor Int) `shouldBe`
        rebuildTreeCursor (func lec)

treeShouldBe ::
       (Show a, Eq a) => STC.TreeCursor a -> STC.TreeCursor a -> Expectation
treeShouldBe actual expected =
    unless (actual == expected) $
    expectationFailure $
    unlines
        [ "The following should have been equal."
        , "actual:"
        , drawTreeCursor actual
        , "expected:"
        , drawTreeCursor expected
        ]

instance CanFail SwapResult where
    hasFailed (Swapped _) = False
    hasFailed _ = True
    resultIfSucceeded (Swapped a) = Just a
    resultIfSucceeded _ = Nothing

node :: a -> [CTree a] -> CTree a
node a ts = CNode a $ makeCollapse ts
