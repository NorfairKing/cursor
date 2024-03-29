{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.Simple.Tree.DemoteSpec
  ( spec,
  )
where

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Simple.Tree.TestUtils
import Cursor.Tree (TreeCursor (..), closedForest, emptyCForest)
import Data.Tree
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  functorSpec @DemoteResult
  describe "treeCursorDemoteElem" $ do
    it "produces valids on valids" $ producesValid $ treeCursorDemoteElem @Bool
    it "Works on the example from the docs" $
      let promoteStart =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [node 'a' [node 'b' []]],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = [node 'e' []]
                      },
                treeCurrent = 'c',
                treeBelow = closedForest [Node 'd' []]
              }
          promoteEnd =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [node 'b' []],
                        treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = [],
                                treeAboveAbove = Nothing,
                                treeAboveNode = 'p',
                                treeAboveRights = [node 'e' []]
                              },
                        treeAboveNode = 'a',
                        treeAboveRights = [node 'd' []]
                      },
                treeCurrent = 'c',
                treeBelow = emptyCForest
              }
       in case treeCursorDemoteElem promoteStart of
            Demoted tc' -> tc' `treeShouldBe` promoteEnd
            _ -> expectationFailure "treeCursorDemoteElem should not have failed"
    it "demotes the current node to the level of its children" pending
  describe "treeCursorDemoteSubTree" $ do
    it "produces valids on valids" $ producesValid $ treeCursorDemoteSubTree @Bool
    it "Works on the example from the docs" $
      let promoteStart =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [node 'a' [node 'b' []]],
                        treeAboveAbove = Nothing,
                        treeAboveNode = 'p',
                        treeAboveRights = [node 'e' []]
                      },
                treeCurrent = 'c',
                treeBelow = closedForest [Node 'd' []]
              }
          promoteEnd =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [node 'b' []],
                        treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = [],
                                treeAboveAbove = Nothing,
                                treeAboveNode = 'p',
                                treeAboveRights = [node 'e' []]
                              },
                        treeAboveNode = 'a',
                        treeAboveRights = []
                      },
                treeCurrent = 'c',
                treeBelow = closedForest [Node 'd' []]
              }
       in case treeCursorDemoteSubTree promoteStart of
            Demoted tc' -> tc' `treeShouldBe` promoteEnd
            _ -> expectationFailure "treeCursorDemoteSubTree should not have failed"
    it "demotes the current subtree to the level of its children" pending
  describe "treeCursorDemoteElemUnder" $ do
    it "produces valids on valids" $ producesValid3 $ treeCursorDemoteElemUnder @Bool @Bool
    it "Works on the example from the docs" $
      forAllValid $
        \b1 ->
          forAllValid $ \b2 ->
            let demoteStart =
                  TreeCursor
                    { treeAbove =
                        Just
                          TreeAbove
                            { treeAboveLefts = [],
                              treeAboveAbove = Nothing,
                              treeAboveNode = 'p',
                              treeAboveRights = []
                            },
                      treeCurrent = 'a',
                      treeBelow = closedForest [Node 'b' []]
                    }
                demoteEnd =
                  TreeCursor
                    { treeAbove =
                        Just
                          TreeAbove
                            { treeAboveLefts = [],
                              treeAboveAbove =
                                Just
                                  TreeAbove
                                    { treeAboveLefts = [],
                                      treeAboveAbove = Nothing,
                                      treeAboveNode = 'p',
                                      treeAboveRights = [node b2 [node 'b' []]]
                                    },
                              treeAboveNode = b1,
                              treeAboveRights = []
                            },
                      treeCurrent = 'a',
                      treeBelow = emptyCForest
                    }
             in case treeCursorDemoteElemUnder b1 b2 demoteStart of
                  Just tc' -> tc' `treeShouldBe` demoteEnd
                  _ -> expectationFailure "treeCursorDemoteElemUnder should not have failed"
    it "demotes the current node to the level of its children" pending
  describe "treeCursorDemoteSubTreeUnder" $ do
    it "produces valids on valids" $
      producesValid2 $
        treeCursorDemoteSubTreeUnder @Bool @Bool
    it "Works on the example from the docs" $
      forAllValid $
        \v -> do
          let demoteStart =
                TreeCursor
                  { treeAbove = Nothing,
                    treeCurrent = 'a',
                    treeBelow = closedForest [Node 'b' []]
                  }
              demoteEnd =
                TreeCursor
                  { treeAbove =
                      Just
                        TreeAbove
                          { treeAboveLefts = [],
                            treeAboveAbove = Nothing,
                            treeAboveNode = v,
                            treeAboveRights = []
                          },
                    treeCurrent = 'a',
                    treeBelow = closedForest [Node 'b' []]
                  }
          treeCursorDemoteSubTreeUnder v demoteStart `treeShouldBe` demoteEnd
    it "demotes the current subtree to the level of its children, by adding a root" pending
