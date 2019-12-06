{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.Tree.SwapSpec
  ( spec
  ) where

import Test.Hspec

import Test.Validity

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree (TreeAbove(..), TreeCursor(..), emptyCForest)

import Cursor.Simple.Tree.TestUtils

spec :: Spec
spec = do
  functorSpec @SwapResult
  describe "treeCursorSwapPrev" $ do
    it "produces valids on valids" $ producesValidsOnValids $ treeCursorSwapPrev @Bool @Bool
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
            _ -> expectationFailure "treeCursorSwapPrev should not have failed."
    it "reverts treeCursorSwapNext" $
      inverseFunctionsIfSucceedOnValid
        (treeCursorSwapNext @Bool @Bool)
        (treeCursorSwapPrev @Bool @Bool)
    it "swaps the current node with the previous node" pending
  describe "treeCursorSwapNext" $ do
    it "produces valids on valids" $ producesValidsOnValids $ treeCursorSwapNext @Bool @Bool
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
            _ -> expectationFailure "treeCursorSwapNext should not have failed."
    it "reverts treeCursorSwapNext" $
      inverseFunctionsIfSucceedOnValid
        (treeCursorSwapPrev @Bool @Bool)
        (treeCursorSwapNext @Bool @Bool)
    it "swaps the current node with the next node" pending
