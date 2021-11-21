{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.Simple.Tree.SwapSpec
  ( spec,
  )
where

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Simple.Tree.TestUtils
import Cursor.Tree (TreeCursor (..), emptyCForest)
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  functorSpec @SwapResult
  describe "treeCursorSwapPrev" $ do
    it "produces valids on valids" $ producesValid $ treeCursorSwapPrev @Bool @Bool
    it "works on the example from the docs" $
      let start =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [node 'a' []],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = []
                      },
                treeCurrent = 'b',
                treeBelow = emptyCForest
              }
          end =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = [node 'a' []]
                      },
                treeCurrent = 'b',
                treeBelow = emptyCForest
              }
       in case treeCursorSwapPrev start of
            Swapped r -> r `treeShouldBe` end
            _ -> expectationFailure "treeCursorSwapPrev should not have failed."
    it "reverts treeCursorSwapNext" $
      inverseFunctionsIfSucceed
        (treeCursorSwapNext @Bool @Bool)
        (treeCursorSwapPrev @Bool @Bool)
    it "swaps the current node with the previous node" pending
  describe "treeCursorSwapNext" $ do
    it "produces valids on valids" $ producesValid $ treeCursorSwapNext @Bool @Bool
    it "works on the example from the docs" $
      let start =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = [node 'b' []]
                      },
                treeCurrent = 'a',
                treeBelow = emptyCForest
              }
          end =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [node 'b' []],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = []
                      },
                treeCurrent = 'a',
                treeBelow = emptyCForest
              }
       in case treeCursorSwapNext start of
            Swapped r -> r `treeShouldBe` end
            _ -> expectationFailure "treeCursorSwapNext should not have failed."
    it "reverts treeCursorSwapNext" $
      inverseFunctionsIfSucceed
        (treeCursorSwapPrev @Bool @Bool)
        (treeCursorSwapNext @Bool @Bool)
    it "swaps the current node with the next node" pending
