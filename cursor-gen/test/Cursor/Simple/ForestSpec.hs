{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.ForestSpec
    ( spec
    ) where

import Test.Hspec

import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Control.Monad (unless)
import Data.Tree

import Cursor.Forest (ForestCursor(..))
import Cursor.List.NonEmpty
import Cursor.Simple.Forest hiding (ForestCursor)
import qualified Cursor.Simple.Forest as SFC (ForestCursor)
import Cursor.Tree
import Cursor.Types

import Cursor.Simple.Forest.Gen ()

spec :: Spec
spec = do
    eqSpec @(SFC.ForestCursor Int)
    genValidSpec @(SFC.ForestCursor Double)
    describe "makeForestCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeForestCursor @Double)
    describe "rebuildForestCursor" $ do
        it "produces valid forests" $
            producesValidsOnValids (rebuildForestCursor @Double)
        it "is the inverse of makeForestCursor for integers" $
            inverseFunctions (makeForestCursor @Int) rebuildForestCursor
    describe "forestCursorLestCursorL" $
        lensSpecOnValid (forestCursorListCursorL @Double)
    describe "forestCursorSelectedTreeL" $
        lensSpecOnValid (forestCursorSelectedTreeL @Double)
    describe "forestCursorSelection" $ do
        it "produces valid ints" $
            producesValidsOnValids (forestCursorSelection @Double)
        it "returns the index of the currently selected element" pending
    describe "forestCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorSelectIndex @Double)
        it "is the identity function when given the current selection" $
            forAllValid $ \fc ->
                forestCursorSelectIndex (forestCursorSelection fc) fc `shouldBe`
                Just (fc :: SFC.ForestCursor Double)
        it "returns selects the element at the given index" pending
    movementsSpec
    insertSpec
    deleteSpec
    shiftingSpec

movementsSpec :: Spec
movementsSpec = do
    describe "forestCursorSelectPrevTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectPrevTreeCursor @Double
        it "is a movement" $ isMovementM forestCursorSelectPrevTreeCursor
        it "selects the previous tree cursor" pending
    describe "forestCursorSelectNextTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectNextTreeCursor @Double
        it "is a movement" $ isMovementM forestCursorSelectNextTreeCursor
        it "selects the next tree" pending
    describe "forestCursorSelectFirstTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectFirstTreeCursor @Double
        it "is a movement" $ isMovement forestCursorSelectFirstTreeCursor
        it "selects the first tree" pending
    describe "forestCursorSelectLastTreeCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectLastTreeCursor @Double
        it "is a movement" $ isMovement forestCursorSelectLastTreeCursor
        it "selects the last tree" pending
    describe "forestCursorSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectPrev @Double
        it "is a movement" $ isMovementM forestCursorSelectPrev
        it "selects the previous node" pending
        it "Works for this classic example" $
            --   > 1
            --     > 2 <- expected end cursor
            --   > 3 <- start cursor
         do
            let start =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = [Node 1 [Node 2 []]]
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove = Nothing
                                            , treeCurrent = 3 :: Int
                                            , treeBelow = []
                                            }
                                  , nonEmptyCursorNext = []
                                  }
                        }
                expected =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = []
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove =
                                                  Just
                                                      TreeAbove
                                                          { treeAboveAbove =
                                                                Nothing
                                                          , treeAboveLefts = []
                                                          , treeAboveNode = 1
                                                          , treeAboveRights = []
                                                          }
                                            , treeCurrent = 2
                                            , treeBelow = []
                                            }
                                  , nonEmptyCursorNext = [Node 3 []]
                                  }
                        }
            case forestCursorSelectPrev start of
                Nothing ->
                    expectationFailure
                        "forestCursorSelectPrev should not have failed."
                Just actual -> actual `forestShouldBe` expected
    describe "forestCursorSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectNext @Double
        it "is a movement" $ isMovementM forestCursorSelectNext
        it "selects the next node" pending
        it "Works for this classic example" $
            --   > 1
            --     > 2 <- start cursor
            --   > 3 <- expected end cursor
         do
            let start =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = []
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove =
                                                  Just
                                                      TreeAbove
                                                          { treeAboveAbove =
                                                                Nothing
                                                          , treeAboveLefts = []
                                                          , treeAboveNode = 1
                                                          , treeAboveRights = []
                                                          }
                                            , treeCurrent = 2
                                            , treeBelow = []
                                            }
                                  , nonEmptyCursorNext = [Node 3 []]
                                  }
                        }
                expected =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = [Node 1 [Node 2 []]]
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove = Nothing
                                            , treeCurrent = 3 :: Int
                                            , treeBelow = []
                                            }
                                  , nonEmptyCursorNext = []
                                  }
                        }
            case forestCursorSelectNext start of
                Nothing ->
                    expectationFailure
                        "forestCursorSelectNext should not have failed."
                Just actual -> actual `forestShouldBe` expected
    describe "forestCursorSelectPrevOnSameLevel" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectPrevOnSameLevel @Double
        it "is a movement" $ isMovementM forestCursorSelectPrevOnSameLevel
        it
            "selects the previous node on the same level as the current node"
            pending
    describe "forestCursorSelectNextOnSameLevel" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectNextOnSameLevel @Double
        it "is a movement" $ isMovementM forestCursorSelectNextOnSameLevel
        it "selects the next node on the same level as the current node" pending
    describe "forestCursorSelectBelowAtPos" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 $ forestCursorSelectBelowAtPos @Double
        it "is a movement for any index" $
            forAllValid $ \i -> isMovementM $ forestCursorSelectBelowAtPos i
        it
            "selects the child of the selected node at the given position"
            pending
    describe "forestCursorSelectBelowAtStart" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectBelowAtStart @Double
        it "is a movement" $ isMovementM forestCursorSelectBelowAtStart
        it "selects the first child of the selected node" pending
    describe "forestCursorSelectBelowAtEnd" $ do
        it "produces valid cursors" $
            producesValidsOnValids $ forestCursorSelectBelowAtEnd @Double
        it "is a movement" $ isMovementM forestCursorSelectBelowAtEnd
        it "selects the first child of the selected node" pending

insertSpec :: Spec
insertSpec = do
    describe "forestCursorInsertEntireTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorInsertEntireTree @Double)
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
    describe "forestCursorAppendEntireTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAppendEntireTree @Double)
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
    describe "forestCursorAddChildTreeToNodeAtPos" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 $
            forestCursorAddChildTreeToNodeAtPos @Double
        it
            "adds a child tree to a node at the given position in the children of that node"
            pending
    describe "forestCursorAddChildTreeToNodeAtStart" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 $
            forestCursorAddChildTreeToNodeAtStart @Double
        it
            "adds a child tree to a node at the start the children of that node"
            pending
    describe "forestCursorAddChildTreeToNodeAtEnd" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 $
            forestCursorAddChildTreeToNodeAtEnd @Double
        it
            "adds a child tree to a node at the end the children of that node"
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
    describe "forestCursorAddRoot" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (forestCursorAddRoot @Double)
        it "houses the entire forest under the given node" pending

deleteSpec :: Spec
deleteSpec = do
    describe "forestCursorRemoveElemAndSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorRemoveElemAndSelectPrev @Double)
        it "works for this simple example" $
            forAllValid $ \fs ->
                let simpleDeleteElemStart =
                        ForestCursor
                            { forestCursorListCursor =
                                  NonEmptyCursor
                                      { nonEmptyCursorPrev = []
                                      , nonEmptyCursorCurrent =
                                            TreeCursor
                                                { treeAbove = Nothing
                                                , treeCurrent = 1 :: Int
                                                , treeBelow = [Node 2 fs]
                                                }
                                      , nonEmptyCursorNext = []
                                      }
                            }
                 in case forestCursorRemoveElemAndSelectPrev
                             simpleDeleteElemStart of
                        Nothing -> pure ()
                        Just Deleted ->
                            expectationFailure
                                "forestCursorRemoveElemAndSelectPrev should not have deleted the entire example forest."
                        Just (Updated _) ->
                            expectationFailure
                                "forestCursorRemoveElemAndSelectPrev should not have updated the forest cursor, but failed instead."
        it
            "removes the selected element and selects the previous element"
            pending
    describe "forestCursorDeleteElemAndSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorDeleteElemAndSelectNext @Double)
        it "works for this simple example" $
            forAllValid $ \fs ->
                let simpleDeleteElemStart =
                        ForestCursor
                            { forestCursorListCursor =
                                  NonEmptyCursor
                                      { nonEmptyCursorPrev = []
                                      , nonEmptyCursorCurrent =
                                            TreeCursor
                                                { treeAbove = Nothing
                                                , treeCurrent = 1
                                                , treeBelow = [Node 2 fs]
                                                }
                                      , nonEmptyCursorNext = []
                                      }
                            }
                    simpleDeleteElemExpected =
                        ForestCursor
                            { forestCursorListCursor =
                                  NonEmptyCursor
                                      { nonEmptyCursorPrev = []
                                      , nonEmptyCursorCurrent =
                                            TreeCursor
                                                { treeAbove = Nothing
                                                , treeCurrent = 2 :: Int
                                                , treeBelow = fs
                                                }
                                      , nonEmptyCursorNext = []
                                      }
                            }
                 in case forestCursorDeleteElemAndSelectNext
                             simpleDeleteElemStart of
                        Nothing ->
                            expectationFailure
                                "forestCursorDeleteElemAndSelectNext should not have failed."
                        Just Deleted ->
                            expectationFailure
                                "forestCursorDeleteElemAndSelectNext should not have deleted the entire example forest."
                        Just (Updated f) ->
                            f `shouldBe` simpleDeleteElemExpected
        it "deletes the selected element and selects the next element" pending
    describe "forestCursorRemoveElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorRemoveElem @Double)
        it "works for this simple example" $
            forAllValid $ \fs ->
                let simpleDeleteElemStart =
                        ForestCursor
                            { forestCursorListCursor =
                                  NonEmptyCursor
                                      { nonEmptyCursorPrev = []
                                      , nonEmptyCursorCurrent =
                                            TreeCursor
                                                { treeAbove = Nothing
                                                , treeCurrent = 1
                                                , treeBelow = [Node 2 fs]
                                                }
                                      , nonEmptyCursorNext = []
                                      }
                            }
                    simpleDeleteElemExpected =
                        ForestCursor
                            { forestCursorListCursor =
                                  NonEmptyCursor
                                      { nonEmptyCursorPrev = []
                                      , nonEmptyCursorCurrent =
                                            TreeCursor
                                                { treeAbove = Nothing
                                                , treeCurrent = 2 :: Int
                                                , treeBelow = fs
                                                }
                                      , nonEmptyCursorNext = []
                                      }
                            }
                 in case forestCursorRemoveElem simpleDeleteElemStart of
                        Deleted ->
                            expectationFailure
                                "forestCursorRemoveElem should not have deleted the entire example forest."
                        Updated f -> f `shouldBe` simpleDeleteElemExpected
        it "removes the selected element" pending
    describe "forestCursorDeleteElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorDeleteElem @Double)
        it "works for this simple example" $
            forAllValid $ \fs ->
                let simpleDeleteElemStart =
                        ForestCursor
                            { forestCursorListCursor =
                                  NonEmptyCursor
                                      { nonEmptyCursorPrev = []
                                      , nonEmptyCursorCurrent =
                                            TreeCursor
                                                { treeAbove = Nothing
                                                , treeCurrent = 1
                                                , treeBelow = [Node 2 fs]
                                                }
                                      , nonEmptyCursorNext = []
                                      }
                            }
                    simpleDeleteElemExpected =
                        ForestCursor
                            { forestCursorListCursor =
                                  NonEmptyCursor
                                      { nonEmptyCursorPrev = []
                                      , nonEmptyCursorCurrent =
                                            TreeCursor
                                                { treeAbove = Nothing
                                                , treeCurrent = 2 :: Int
                                                , treeBelow = fs
                                                }
                                      , nonEmptyCursorNext = []
                                      }
                            }
                 in case forestCursorDeleteElem simpleDeleteElemStart of
                        Deleted ->
                            expectationFailure
                                "forestCursorDeleteElem should not have deleted the entire example forest."
                        Updated f -> f `shouldBe` simpleDeleteElemExpected
        it "deletes the selected element" pending
    describe "forestCursorRemoveSubTreeAndSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (forestCursorRemoveSubTreeAndSelectPrev @Double)
        it "removes the selected subtree and selects the previous tree" pending
    describe "forestCursorDeleteSubTreeAndSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (forestCursorDeleteSubTreeAndSelectNext @Double)
        it "deletes the selected subtree and selects the next tree" pending
    describe "forestCursorRemoveSubTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorRemoveSubTree @Double)
        it "removes the selected subtree" pending
    describe "forestCursorDeleteSubTree" $ do
        it "produces valid cursors" $
            producesValidsOnValids (forestCursorDeleteSubTree @Double)
        it "deletes the selected subtree" pending

shiftingSpec :: Spec
shiftingSpec = do
    describe "forestCursorPromoteElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ forestCursorPromoteElem @Double
        it "works on the example from the documentation" $
            let start =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = []
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove =
                                                  Just
                                                      TreeAbove
                                                          { treeAboveLefts =
                                                                [ Node
                                                                      'b'
                                                                      [ Node
                                                                            'c'
                                                                            []
                                                                      ]
                                                                ]
                                                          , treeAboveAbove =
                                                                Nothing
                                                          , treeAboveNode = 'a'
                                                          , treeAboveRights =
                                                                [ Node
                                                                      'f'
                                                                      [ Node
                                                                            'g'
                                                                            []
                                                                      ]
                                                                ]
                                                          }
                                            , treeCurrent = 'd'
                                            , treeBelow = [Node 'e' []]
                                            }
                                  , nonEmptyCursorNext = [Node 'h' []]
                                  }
                        }
                expected =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev =
                                        [ Node
                                              'a'
                                              [ Node
                                                    'b'
                                                    [Node 'c' [], Node 'e' []]
                                              , Node 'f' [Node 'g' []]
                                              ]
                                        ]
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove = Nothing
                                            , treeCurrent = 'd'
                                            , treeBelow = []
                                            }
                                  , nonEmptyCursorNext = [Node 'h' []]
                                  }
                        }
             in case forestCursorPromoteElem start of
                    Nothing ->
                        expectationFailure
                            "forestCursorPromoteElem should not have failed."
                    Just f -> f `shouldBe` expected
        it "promotes the current node to the level of its parent" pending
    describe "forestCursorDemoteElem" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ forestCursorDemoteElem @Double
        it "works on the example from the documentation" $
            let start =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev =
                                        [Node 'a' [Node 'b' []]]
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove = Nothing
                                            , treeCurrent = 'c'
                                            , treeBelow = [Node 'd' []]
                                            }
                                  , nonEmptyCursorNext = [Node 'e' []]
                                  }
                        }
                expected =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = []
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove =
                                                  Just
                                                      TreeAbove
                                                          { treeAboveLefts =
                                                                [Node 'b' []]
                                                          , treeAboveAbove =
                                                                Nothing
                                                          , treeAboveNode = 'a'
                                                          , treeAboveRights =
                                                                [Node 'd' []]
                                                          }
                                            , treeCurrent = 'c'
                                            , treeBelow = []
                                            }
                                  , nonEmptyCursorNext = [Node 'e' []]
                                  }
                        }
             in case forestCursorDemoteElem start of
                    Nothing ->
                        expectationFailure
                            "forestCursorDemoteElem should not have failed."
                    Just f -> f `forestShouldBe` expected
        it "demotes the current node to the level of its children" pending
    describe "forestCursorPromoteSubTree" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ forestCursorPromoteSubTree @Double
        it "works on the example from the documentation" $
            let start =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = []
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove =
                                                  Just
                                                      TreeAbove
                                                          { treeAboveLefts =
                                                                [ Node
                                                                      'b'
                                                                      [ Node
                                                                            'c'
                                                                            []
                                                                      ]
                                                                ]
                                                          , treeAboveAbove =
                                                                Nothing
                                                          , treeAboveNode = 'a'
                                                          , treeAboveRights =
                                                                [ Node
                                                                      'f'
                                                                      [ Node
                                                                            'g'
                                                                            []
                                                                      ]
                                                                ]
                                                          }
                                            , treeCurrent = 'd'
                                            , treeBelow = [Node 'e' []]
                                            }
                                  , nonEmptyCursorNext = [Node 'h' []]
                                  }
                        }
                expected =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev =
                                        [ Node
                                              'a'
                                              [ Node 'b' [Node 'c' []]
                                              , Node 'f' [Node 'g' []]
                                              ]
                                        ]
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove = Nothing
                                            , treeCurrent = 'd'
                                            , treeBelow = [Node 'e' []]
                                            }
                                  , nonEmptyCursorNext = [Node 'h' []]
                                  }
                        }
             in case forestCursorPromoteSubTree start of
                    Nothing ->
                        expectationFailure
                            "forestCursorPromoteSubTree should not have failed."
                    Just f -> f `shouldBe` expected
        it "promotes the current subtree to the level of its parent" pending
    describe "forestCursorDemoteSubTree" $ do
        it "produces valids on valids" $
            producesValidsOnValids $ forestCursorDemoteSubTree @Double
        it "works on the example from the documentation" $
            let start =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev =
                                        [Node 'a' [Node 'b' []]]
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove = Nothing
                                            , treeCurrent = 'c'
                                            , treeBelow = [Node 'd' []]
                                            }
                                  , nonEmptyCursorNext = []
                                  }
                        }
                expected =
                    ForestCursor
                        { forestCursorListCursor =
                              NonEmptyCursor
                                  { nonEmptyCursorPrev = []
                                  , nonEmptyCursorCurrent =
                                        TreeCursor
                                            { treeAbove =
                                                  Just
                                                      TreeAbove
                                                          { treeAboveLefts =
                                                                [Node 'b' []]
                                                          , treeAboveAbove =
                                                                Nothing
                                                          , treeAboveNode = 'a'
                                                          , treeAboveRights = []
                                                          }
                                            , treeCurrent = 'c'
                                            , treeBelow = [Node 'd' []]
                                            }
                                  , nonEmptyCursorNext = []
                                  }
                        }
             in case forestCursorDemoteSubTree start of
                    Nothing ->
                        expectationFailure
                            "forestCursorDemoteSubTree should not have failed."
                    Just f -> f `forestShouldBe` expected
        it "demotes the current subtree to the level of its children" pending

isMovementM ::
       (forall a. SFC.ForestCursor a -> Maybe (SFC.ForestCursor a)) -> Property
isMovementM func =
    forAllValid @(SFC.ForestCursor Int) $ \lec ->
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

isMovement :: (forall a. SFC.ForestCursor a -> SFC.ForestCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildForestCursor (lec :: SFC.ForestCursor Int) `shouldBe`
        rebuildForestCursor (func lec)

forestShouldBe ::
       (Show a, Eq a) => SFC.ForestCursor a -> SFC.ForestCursor a -> Expectation
forestShouldBe actual expected =
    unless (actual == expected) $
    expectationFailure $
    unlines
        [ "The following should have been equal."
        , "actual:"
        , drawForestCursor actual
        , "expected:"
        , drawForestCursor expected
        ]
