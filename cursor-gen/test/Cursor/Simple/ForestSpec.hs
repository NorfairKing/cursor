{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.ForestSpec
  ( spec
  ) where

import Data.Tree

import Control.Monad (unless)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

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
  genValidSpec @(SFC.ForestCursor Rational)
  modifyMaxSize (`quot` 2) $
    modifyMaxSuccess (`quot` 2) $
    shrinkValidSpecWithLimit @(SFC.ForestCursor Rational) 10
  describe "makeForestCursor" $
    it "produces valid cursors" $
    producesValidsOnValids (makeForestCursor @Rational)
  describe "rebuildForestCursor" $ do
    it "produces valid forests" $
      producesValidsOnValids (rebuildForestCursor @Rational)
    it "is the inverse of makeForestCursor for integers" $
      inverseFunctions (makeForestCursor @Int) rebuildForestCursor
  describe "forestCursorLestCursorL" $
    lensSpecOnValid (forestCursorListCursorL @Rational @Rational)
  describe "forestCursorSelectedTreeL" $
    lensSpecOnValid (forestCursorSelectedTreeL @Rational @Rational)
  describe "forestCursorSelection" $ do
    it "produces valid ints" $
      producesValidsOnValids (forestCursorSelection @Rational @Rational)
    it "returns the index of the currently selected element" pending
  describe "forestCursorSelectIndex" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorSelectIndex @Rational)
    it "is the identity function when given the current selection" $
      forAllValid $ \fc ->
        forestCursorSelectIndex (forestCursorSelection fc) fc `shouldBe`
        Just (fc :: SFC.ForestCursor Rational)
    it "returns selects the element at the given index" pending
  movementsSpec
  collapseSpec
  insertSpec
  swapSpec
  deleteSpec
  shiftingSpec

movementsSpec :: Spec
movementsSpec = do
  describe "forestCursorSelectPrevTreeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectPrevTreeCursor @Rational
    it "is a movement" $ isMovementM forestCursorSelectPrevTreeCursor
    it "selects the previous tree cursor" pending
  describe "forestCursorSelectNextTreeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectNextTreeCursor @Rational
    it "is a movement" $ isMovementM forestCursorSelectNextTreeCursor
    it "selects the next tree" pending
  describe "forestCursorSelectFirstTreeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectFirstTreeCursor @Rational
    it "is a movement" $ isMovement forestCursorSelectFirstTreeCursor
    it "selects the first tree" pending
  describe "forestCursorSelectLastTreeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectLastTreeCursor @Rational
    it "is a movement" $ isMovement forestCursorSelectLastTreeCursor
    it "selects the last tree" pending
  describe "forestCursorSelectPrev" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectPrev @Rational
    it "is a movement" $ isMovementM forestCursorSelectPrev
    it "selects the previous node" pending
        -- TODO example with a collapsed tree
    it "Works for this classic example without any collapsing" $
            --   > 1
            --     > 2 <- expected end cursor
            --   > 3 <- start cursor
     do
      let start =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev =
                        [CNode 1 $ openForest [CNode 2 $ emptyCForest]]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 3 :: Int
                          , treeBelow = emptyCForest
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
                                  { treeAboveAbove = Nothing
                                  , treeAboveLefts = []
                                  , treeAboveNode = 1
                                  , treeAboveRights = []
                                  }
                          , treeCurrent = 2
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = [CNode 3 $ emptyCForest]
                    }
              }
      case forestCursorSelectPrev start of
        Nothing ->
          expectationFailure "forestCursorSelectPrev should not have failed."
        Just actual -> actual `forestShouldBe` expected
  describe "forestCursorSelectNext" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectNext @Rational
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
                                  { treeAboveAbove = Nothing
                                  , treeAboveLefts = []
                                  , treeAboveNode = 1
                                  , treeAboveRights = []
                                  }
                          , treeCurrent = 2
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = [CNode 3 $ emptyCForest]
                    }
              }
          expected =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev =
                        [CNode 1 $ openForest [CNode 2 $ emptyCForest]]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 3 :: Int
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = []
                    }
              }
      case forestCursorSelectNext start of
        Nothing ->
          expectationFailure "forestCursorSelectNext should not have failed."
        Just actual -> actual `forestShouldBe` expected
  describe "forestCursorSelectPrevOnSameLevel" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectPrevOnSameLevel @Rational
    it "is a movement" $ isMovementM forestCursorSelectPrevOnSameLevel
    it "selects the previous node on the same level as the current node" pending
  describe "forestCursorSelectNextOnSameLevel" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectNextOnSameLevel @Rational
    it "is a movement" $ isMovementM forestCursorSelectNextOnSameLevel
    it "selects the next node on the same level as the current node" pending
  describe "forestCursorSelectFirst" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectFirst @Rational
    it "is a movement" $ isMovement forestCursorSelectFirst
    it "selects the first node in the forest" pending
  describe "forestCursorSelectLast" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectLast @Rational
    it "is a movement" $ isMovement forestCursorSelectLast
    it "selects the last node in the forest" pending
  describe "forestCursorSelectAbove" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectAbove @Rational
    it "is a movement" $ isMovementM forestCursorSelectAbove
    it "selects the parent" pending
  describe "forestCursorSelectBelowAtPos" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 $ forestCursorSelectBelowAtPos @Rational
    it "is a movement for any index" $
      forAllValid $ \i -> isMovementM $ forestCursorSelectBelowAtPos i
    it "selects the child of the selected node at the given position" pending
  describe "forestCursorSelectBelowAtStart" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectBelowAtStart @Rational
    it "is a movement" $ isMovementM forestCursorSelectBelowAtStart
    it "selects the first child of the selected node" pending
  describe "forestCursorSelectBelowAtEnd" $ do
    it "produces valid cursors" $
      producesValidsOnValids $ forestCursorSelectBelowAtEnd @Rational
    it "is a movement" $ isMovementM forestCursorSelectBelowAtEnd
    it "selects the first child of the selected node" pending

collapseSpec :: Spec
collapseSpec = do
  describe "forestCursorOpenCurrentForest" $
    it "produces valid cursors" $
    producesValidsOnValids $ forestCursorOpenCurrentForest @Rational @Rational
  describe "forestCursorCloseCurrentForest" $
    it "produces valid cursors" $
    producesValidsOnValids $ forestCursorCloseCurrentForest @Rational @Rational
  describe "forestCursorToggleCurrentForest" $
    it "produces valid cursors" $
    producesValidsOnValids $ forestCursorToggleCurrentForest @Double @Double
  describe "forestCursorOpenCurrentForestRecursively" $
    it "produces valid cursors" $
    producesValidsOnValids $ forestCursorToggleCurrentForest @Rational @Rational
  describe "forestCursorToggleCurrentForestRecursively" $
    it "produces valid cursors" $
    producesValidsOnValids $
    forestCursorToggleCurrentForestRecursively @Rational @Rational

insertSpec :: Spec
insertSpec = do
  describe "forestCursorInsertEntireTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorInsertEntireTree @Rational @Rational)
    it "inserts a tree cursor before the currently selected tree cursor" pending
  describe "forestCursorInsertAndSelectTreeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorInsertAndSelectTreeCursor @Rational)
    it
      "inserts a tree cursor before the currently selected tree cursor and selects it"
      pending
  describe "forestCursorAppendEntireTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorAppendEntireTree @Rational @Rational)
    it "appends a tree after the currently selected tree cursor" pending
  describe "forestCursorAppendAndSelectTreeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorAppendAndSelectTreeCursor @Rational)
    it
      "appends a tree cursor after the currently selected tree cursor and selects it"
      pending
  describe "forestCursorInsertTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorInsertTree @Rational @Rational)
    it "inserts a tree before the currently selected tree" pending
  describe "forestCursorInsertAndSelectTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorInsertAndSelectTree @Rational)
    it
      "inserts a tree before the currently selected tree and selects it"
      pending
  describe "forestCursorAppendTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorAppendTree @Rational @Rational)
    it "appends a tree after the currently selected tree " pending
  describe "forestCursorAppendAndSelectTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorAppendAndSelectTree @Rational)
    it "appends a tree after the currently selected tree and selects it" pending
  describe "forestCursorInsert" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorInsert @Rational @Rational)
    it "inserts a node before the currently selected node" pending
  describe "forestCursorInsertAndSelect" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorInsertAndSelect @Rational)
    it
      "inserts a node before the currently selected node and selects it"
      pending
  describe "forestCursorAppend" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorAppend @Rational @Rational)
    it "appends a node after the currently selected node" pending
  describe "forestCursorAppendAndSelect" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorAppendAndSelect @Rational)
    it "appends a node after the currently selected node and selects it" pending
  describe "forestCursorAddChildTreeToNodeAtPos" $ do
    it "produces valid cursors" $
      producesValidsOnValids3 $
      forestCursorAddChildTreeToNodeAtPos @Rational @Rational
    it
      "adds a child tree to a node at the given position in the children of that node"
      pending
  describe "forestCursorAddChildTreeToNodeAtStart" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 $
      forestCursorAddChildTreeToNodeAtStart @Rational @Rational
    it
      "adds a child tree to a node at the start the children of that node"
      pending
  describe "forestCursorAddChildTreeToNodeAtEnd" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 $
      forestCursorAddChildTreeToNodeAtEnd @Rational @Rational
    it
      "adds a child tree to a node at the end the children of that node"
      pending
  describe "forestCursorAddChildToNodeAtPos" $ do
    it "produces valid cursors" $
      producesValidsOnValids3 $
      forestCursorAddChildToNodeAtPos @Rational @Rational
    it
      "adds a child to a node at the given position in the children of that node"
      pending
  describe "forestCursorAddChildToNodeAtStart" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 $
      forestCursorAddChildToNodeAtStart @Rational @Rational
    it "adds a child to a node at the start the children of that node" pending
  describe "forestCursorAddChildToNodeAtEnd" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 $
      forestCursorAddChildToNodeAtEnd @Rational @Rational
    it "adds a child to a node at the end the children of that node" pending
  describe "forestCursorAddRoot" $ do
    it "produces valid cursors" $
      producesValidsOnValids2 (forestCursorAddRoot @Rational)
    it "houses the entire forest under the given node" pending

swapSpec :: Spec
swapSpec = do
  describe "forestCursorSwapPrev" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorSwapPrev @Rational @Rational)
    it "works on the example from the docs" $
      let start =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev = [CNode 'a' $ emptyCForest]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'b'
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = []
                    }
              }
          end =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev = []
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'b'
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = [CNode 'a' $ emptyCForest]
                    }
              }
       in case forestCursorSwapPrev start of
            Nothing ->
              expectationFailure "forestCursorSwapPrev should not have failed."
            Just r -> r `forestShouldBe` end
    it "swaps the current node with the previous node on the same level" pending
    it "reverts forestCursorSwapNext" $
      inverseFunctionsIfSucceedOnValid
        (forestCursorSwapNext @Rational @Rational)
        (forestCursorSwapPrev @Rational @Rational)
  describe "forestCursorSwapNext" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorSwapNext @Rational @Rational)
    it "works on the example from the docs" $
      let start =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev = []
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'a'
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = [CNode 'b' $ emptyCForest]
                    }
              }
          end =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev = [CNode 'b' $ emptyCForest]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'a'
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = []
                    }
              }
       in case forestCursorSwapNext start of
            Nothing ->
              expectationFailure "forestCursorSwapNext should not have failed."
            Just r -> r `forestShouldBe` end
    it "swaps the current node with the next node on the same level" pending
    it "reverts forestCursorSwapPrev" $
      inverseFunctionsIfSucceedOnValid
        (forestCursorSwapPrev @Rational @Rational)
        (forestCursorSwapNext @Rational @Rational)

deleteSpec :: Spec
deleteSpec = do
  describe "forestCursorRemoveElemAndSelectPrev" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorRemoveElemAndSelectPrev @Rational)
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
                            , treeBelow = closedForest [Node 2 fs]
                            }
                      , nonEmptyCursorNext = []
                      }
                }
         in case forestCursorRemoveElemAndSelectPrev simpleDeleteElemStart of
              Nothing -> pure ()
              Just Deleted ->
                expectationFailure
                  "forestCursorRemoveElemAndSelectPrev should not have deleted the entire example forest."
              Just (Updated _) ->
                expectationFailure
                  "forestCursorRemoveElemAndSelectPrev should not have updated the forest cursor, but failed instead."
    it "removes the selected element and selects the previous element" pending
  describe "forestCursorDeleteElemAndSelectNext" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorDeleteElemAndSelectNext @Rational)
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
                            , treeBelow = closedForest [Node 2 fs]
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
                            , treeBelow = closedForest fs
                            }
                      , nonEmptyCursorNext = []
                      }
                }
         in case forestCursorDeleteElemAndSelectNext simpleDeleteElemStart of
              Nothing ->
                expectationFailure
                  "forestCursorDeleteElemAndSelectNext should not have failed."
              Just Deleted ->
                expectationFailure
                  "forestCursorDeleteElemAndSelectNext should not have deleted the entire example forest."
              Just (Updated f) -> f `shouldBe` simpleDeleteElemExpected
    it "deletes the selected element and selects the next element" pending
  describe "forestCursorRemoveElem" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorRemoveElem @Rational)
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
                            , treeBelow = closedForest [Node 2 fs]
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
                            , treeBelow = closedForest fs
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
      producesValidsOnValids (forestCursorDeleteElem @Rational)
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
                            , treeBelow = closedForest [Node 2 fs]
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
                            , treeBelow = closedForest fs
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
      producesValidsOnValids (forestCursorRemoveSubTreeAndSelectPrev @Rational)
    it "removes the selected subtree and selects the previous tree" pending
  describe "forestCursorDeleteSubTreeAndSelectNext" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorDeleteSubTreeAndSelectNext @Rational)
    it "deletes the selected subtree and selects the next tree" pending
  describe "forestCursorRemoveSubTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorRemoveSubTree @Rational)
    it "removes the selected subtree" pending
  describe "forestCursorDeleteSubTree" $ do
    it "produces valid cursors" $
      producesValidsOnValids (forestCursorDeleteSubTree @Rational)
    it "deletes the selected subtree" pending

shiftingSpec :: Spec
shiftingSpec = do
  describe "forestCursorPromoteElem" $ do
    it "produces valids on valids" $
      producesValidsOnValids $ forestCursorPromoteElem @Rational
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
                                      [CNode 'b' $ closedForest [Node 'c' []]]
                                  , treeAboveAbove = Nothing
                                  , treeAboveNode = 'a'
                                  , treeAboveRights =
                                      [CNode 'f' $ closedForest [Node 'g' []]]
                                  }
                          , treeCurrent = 'd'
                          , treeBelow = closedForest [Node 'e' []]
                          }
                    , nonEmptyCursorNext = [CNode 'h' $ emptyCForest]
                    }
              }
          expected =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev =
                        [ CNode 'a' $
                          openForest
                            [ CNode 'b' $
                              openForest
                                [CNode 'c' emptyCForest, CNode 'e' emptyCForest]
                            , CNode 'f' $ closedForest [Node 'g' []]
                            ]
                        ]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'd'
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = [CNode 'h' emptyCForest]
                    }
              }
       in case forestCursorPromoteElem start of
            Nothing ->
              expectationFailure
                "forestCursorPromoteElem should not have failed."
            Just f -> f `forestShouldBe` expected
    it "promotes the current node to the level of its parent" pending
  describe "forestCursorDemoteElem" $ do
    it "produces valids on valids" $
      producesValidsOnValids $ forestCursorDemoteElem @Rational
    it "works on the example from the documentation" $
      let start =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev =
                        [CNode 'a' $ closedForest [Node 'b' []]]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'c'
                          , treeBelow = closedForest [Node 'd' []]
                          }
                    , nonEmptyCursorNext = [CNode 'e' emptyCForest]
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
                                  { treeAboveLefts = [CNode 'b' $ emptyCForest]
                                  , treeAboveAbove = Nothing
                                  , treeAboveNode = 'a'
                                  , treeAboveRights = [CNode 'd' $ emptyCForest]
                                  }
                          , treeCurrent = 'c'
                          , treeBelow = emptyCForest
                          }
                    , nonEmptyCursorNext = [CNode 'e' $ emptyCForest]
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
      producesValidsOnValids $ forestCursorPromoteSubTree @Rational
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
                                      [CNode 'b' $ closedForest [Node 'c' []]]
                                  , treeAboveAbove = Nothing
                                  , treeAboveNode = 'a'
                                  , treeAboveRights =
                                      [CNode 'f' $ closedForest [Node 'g' []]]
                                  }
                          , treeCurrent = 'd'
                          , treeBelow = closedForest [Node 'e' []]
                          }
                    , nonEmptyCursorNext = [CNode 'h' emptyCForest]
                    }
              }
          expected =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev =
                        [ CNode 'a' $
                          openForest
                            [ CNode 'b' $ closedForest [Node 'c' []]
                            , CNode 'f' $ closedForest [Node 'g' []]
                            ]
                        ]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'd'
                          , treeBelow = closedForest [Node 'e' []]
                          }
                    , nonEmptyCursorNext = [CNode 'h' emptyCForest]
                    }
              }
       in case forestCursorPromoteSubTree start of
            Nothing ->
              expectationFailure
                "forestCursorPromoteSubTree should not have failed."
            Just f -> f `forestShouldBe` expected
    it "promotes the current subtree to the level of its parent" pending
  describe "forestCursorDemoteSubTree" $ do
    it "produces valids on valids" $
      producesValidsOnValids $ forestCursorDemoteSubTree @Rational
    it "works on the example from the documentation" $
      let start =
            ForestCursor
              { forestCursorListCursor =
                  NonEmptyCursor
                    { nonEmptyCursorPrev =
                        [CNode 'a' $ closedForest [Node 'b' []]]
                    , nonEmptyCursorCurrent =
                        TreeCursor
                          { treeAbove = Nothing
                          , treeCurrent = 'c'
                          , treeBelow = closedForest [Node 'd' []]
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
                                  { treeAboveLefts = [CNode 'b' emptyCForest]
                                  , treeAboveAbove = Nothing
                                  , treeAboveNode = 'a'
                                  , treeAboveRights = []
                                  }
                          , treeCurrent = 'c'
                          , treeBelow = closedForest [Node 'd' []]
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
  describe "forestCursorDemoteElemUnder" $ do
    it "produces valids on valids" $
      producesValidsOnValids3 $ forestCursorDemoteElemUnder @Rational @Rational
    it "Works on the example from the docs" $
      forAllValid $ \b1 ->
        forAllValid $ \b2 ->
          let demoteStart =
                ForestCursor $
                NonEmptyCursor
                  { nonEmptyCursorPrev = []
                  , nonEmptyCursorCurrent =
                      TreeCursor
                        { treeAbove = Nothing
                        , treeCurrent = 'a'
                        , treeBelow = closedForest [Node 'b' []]
                        }
                  , nonEmptyCursorNext = []
                  }
              demoteEnd =
                ForestCursor $
                NonEmptyCursor
                  { nonEmptyCursorPrev = []
                  , nonEmptyCursorCurrent =
                      TreeCursor
                        { treeAbove =
                            Just
                              TreeAbove
                                { treeAboveLefts = []
                                , treeAboveAbove = Nothing
                                , treeAboveNode = b1
                                , treeAboveRights = []
                                }
                        , treeCurrent = 'a'
                        , treeBelow = emptyCForest
                        }
                  , nonEmptyCursorNext = [CNode b2 $ closedForest [Node 'b' []]]
                  }
           in forestCursorDemoteElemUnder b1 b2 demoteStart `forestShouldBe`
              demoteEnd
    it "demotes the current node to the level of its children" pending
  describe "forestCursorDemoteSubTreeUnder" $ do
    it "produces valids on valids" $
      producesValidsOnValids2 $
      forestCursorDemoteSubTreeUnder @Rational @Rational
    it "Works on the example from the docs" $
      forAllValid $ \v -> do
        let demoteStart =
              ForestCursor $
              NonEmptyCursor
                { nonEmptyCursorPrev = []
                , nonEmptyCursorCurrent =
                    TreeCursor
                      { treeAbove = Nothing
                      , treeCurrent = 'a'
                      , treeBelow = closedForest [Node 'b' []]
                      }
                , nonEmptyCursorNext = []
                }
            demoteEnd =
              ForestCursor $
              NonEmptyCursor
                { nonEmptyCursorPrev = []
                , nonEmptyCursorCurrent =
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
                      , treeBelow = closedForest [Node 'b' []]
                      }
                , nonEmptyCursorNext = []
                }
        forestCursorDemoteSubTreeUnder v demoteStart `forestShouldBe` demoteEnd
    it
      "demotes the current subtree to the level of its children, by adding a root"
      pending

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
