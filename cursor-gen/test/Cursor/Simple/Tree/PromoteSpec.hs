{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.Simple.Tree.PromoteSpec
  ( spec,
  )
where

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Simple.Tree.TestUtils
import Cursor.Tree
  ( CTree (..),
    TreeAbove (..),
    TreeCursor (..),
    closedForest,
    emptyCForest,
    openForest,
  )
import Data.Tree
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  functorSpec @PromoteElemResult
  applicativeSpec @PromoteElemResult
  monadSpec @PromoteElemResult
  describe "treeCursorPromoteElem" $ do
    it "produces valids on valids" $ producesValidsOnValids $ treeCursorPromoteElem @Bool
    it "Works on the example from the docs" $
      let promoteStart =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [node 'b' [node 'c' []]],
                        treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = [],
                                treeAboveAbove = Nothing,
                                treeAboveNode = 'p',
                                treeAboveRights = [node 'h' []]
                              },
                        treeAboveNode = 'a',
                        treeAboveRights = [node 'f' [node 'g' []]]
                      },
                treeCurrent = 'd',
                treeBelow = closedForest [Node 'e' []]
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
                                    openForest [CNode 'c' emptyCForest, CNode 'e' emptyCForest],
                                  CNode 'f' $ closedForest [Node 'g' []]
                                ]
                          ],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = [CNode 'h' emptyCForest]
                      },
                treeCurrent = 'd',
                treeBelow = emptyCForest
              }
       in case treeCursorPromoteElem promoteStart of
            PromotedElem tc' -> tc' `treeShouldBe` promoteEnd
            _ -> expectationFailure "treeCursorPromoteElem should not have failed"
    it "promotes the current node to the level of its parent" pending
  functorSpec @PromoteResult
  applicativeSpec @PromoteResult
  monadSpec @PromoteResult
  describe "treeCursorPromoteSubTree" $ do
    it "produces valids on valids" $ producesValidsOnValids $ treeCursorPromoteSubTree @Bool
    it "Works on the example from the docs" $
      let promoteStart =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [CNode 'b' $ closedForest [Node 'c' []]],
                        treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = [],
                                treeAboveAbove = Nothing,
                                treeAboveNode = 'p',
                                treeAboveRights = [node 'h' []]
                              },
                        treeAboveNode = 'a',
                        treeAboveRights = [CNode 'f' $ closedForest [Node 'g' []]]
                      },
                treeCurrent = 'd',
                treeBelow = closedForest [Node 'e' []]
              }
          promoteEnd =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts =
                          [ CNode 'a' $
                              openForest
                                [ CNode 'b' $ closedForest [Node 'c' []],
                                  CNode 'f' $ closedForest [Node 'g' []]
                                ]
                          ],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = [CNode 'h' $ closedForest []]
                      },
                treeCurrent = 'd',
                treeBelow = closedForest [Node 'e' []]
              }
       in case treeCursorPromoteSubTree promoteStart of
            Promoted tc' -> tc' `treeShouldBe` promoteEnd
            _ -> expectationFailure "treeCursorPromoteSubTree should not have failed"
    it "promotes the current subtree to the level of its parent" pending
