{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Simple.TreeSpec
    ( spec
    ) where

import Data.Tree

import Test.Hspec

import Test.Validity

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree
       (CForest(..), CTree(..), TreeAbove(..), TreeCursor(..),
        emptyCForest, openForest)
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
                        , treeBelow = ClosedForest [Node 2 fs]
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
                        , treeBelow = ClosedForest [Node 2 fs]
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
                    , treeBelow = emptyCForest
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
                    , treeBelow = emptyCForest
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
                    , treeBelow = emptyCForest
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
                    , treeBelow = emptyCForest
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
                    , treeBelow = ClosedForest [Node 'e' []]
                    }
                promoteEnd =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts =
                                    [ CNode 'a' $
                                      openForest
                                          [ CNode 'b' $
                                            openForest
                                                [ CNode 'c' emptyCForest
                                                , CNode 'e' emptyCForest
                                                ]
                                          , CNode 'f' $
                                            ClosedForest [Node 'g' []]
                                          ]
                                    ]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [CNode 'h' emptyCForest]
                              }
                    , treeCurrent = 'd'
                    , treeBelow = emptyCForest
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
                              { treeAboveLefts =
                                    [CNode 'b' $ ClosedForest [Node 'c' []]]
                              , treeAboveAbove =
                                    Just
                                        TreeAbove
                                        { treeAboveLefts = []
                                        , treeAboveAbove = Nothing
                                        , treeAboveNode = 'p'
                                        , treeAboveRights = [node 'h' []]
                                        }
                              , treeAboveNode = 'a'
                              , treeAboveRights =
                                    [CNode 'f' $ ClosedForest [Node 'g' []]]
                              }
                    , treeCurrent = 'd'
                    , treeBelow = ClosedForest [Node 'e' []]
                    }
                promoteEnd =
                    TreeCursor
                    { treeAbove =
                          Just
                              TreeAbove
                              { treeAboveLefts =
                                    [ CNode 'a' $
                                      openForest
                                          [ CNode 'b' $
                                            ClosedForest [Node 'c' []]
                                          , CNode 'f' $
                                            ClosedForest [Node 'g' []]
                                          ]
                                    ]
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = [CNode 'h' $ ClosedForest []]
                              }
                    , treeCurrent = 'd'
                    , treeBelow = ClosedForest [Node 'e' []]
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
                    , treeBelow = ClosedForest [Node 'd' []]
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
                    , treeBelow = emptyCForest
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
                    , treeBelow = ClosedForest [Node 'd' []]
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
                    , treeBelow = ClosedForest [Node 'd' []]
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
                            , treeBelow = ClosedForest [Node 'b' []]
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
                            , treeBelow = emptyCForest
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
                        , treeBelow = ClosedForest [Node 'b' []]
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
                        , treeBelow = ClosedForest [Node 'b' []]
                        }
                treeCursorDemoteSubTreeUnder v demoteStart `treeShouldBe`
                    demoteEnd
        it
            "demotes the current subtree to the level of its children, by adding a root"
            pending
