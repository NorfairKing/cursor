{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.Tree.DemoteSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import qualified Data.Sequence as S
import Data.Tree

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree (TreeAbove(..), TreeCursor(..), closedForest, emptyCForest)

import Cursor.Simple.Tree.TestUtils

spec :: Spec
spec = do
  functorSpec @DemoteResult
  describe "treeCursorDemoteElem" $ do
    it "produces valids on valids" $ producesValidsOnValids $ treeCursorDemoteElem @Rational
    it "Works on the example from the docs" $
      let promoteStart =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = S.singleton $ node 'a' [node 'b' []]
                      , treeAboveAbove = Nothing
                      , treeAboveNode = 'p'
                      , treeAboveRights = S.singleton $ node 'e' []
                      }
              , treeCurrent = 'c'
              , treeBelow = closedForest [Node 'd' []]
              }
          promoteEnd =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = S.singleton $ node 'b' []
                      , treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = S.empty
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = S.singleton $ node 'e' []
                              }
                      , treeAboveNode = 'a'
                      , treeAboveRights = S.singleton $ node 'd' []
                      }
              , treeCurrent = 'c'
              , treeBelow = emptyCForest
              }
       in case treeCursorDemoteElem promoteStart of
            Demoted tc' -> tc' `treeShouldBe` promoteEnd
            _ -> expectationFailure "treeCursorDemoteElem should not have failed"
    it "demotes the current node to the level of its children" pending
  describe "treeCursorDemoteSubTree" $ do
    it "produces valids on valids" $ producesValidsOnValids $ treeCursorDemoteSubTree @Rational
    it "Works on the example from the docs" $
      let promoteStart =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = S.singleton $ node 'a' [node 'b' []]
                      , treeAboveAbove = Nothing
                      , treeAboveNode = 'p'
                      , treeAboveRights = S.singleton $ node 'e' []
                      }
              , treeCurrent = 'c'
              , treeBelow = closedForest [Node 'd' []]
              }
          promoteEnd =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = S.singleton $ node 'b' []
                      , treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = S.empty
                              , treeAboveAbove = Nothing
                              , treeAboveNode = 'p'
                              , treeAboveRights = S.singleton $ node 'e' []
                              }
                      , treeAboveNode = 'a'
                      , treeAboveRights = S.empty
                      }
              , treeCurrent = 'c'
              , treeBelow = closedForest [Node 'd' []]
              }
       in case treeCursorDemoteSubTree promoteStart of
            Demoted tc' -> tc' `treeShouldBe` promoteEnd
            _ -> expectationFailure "treeCursorDemoteSubTree should not have failed"
    it "demotes the current subtree to the level of its children" pending
  describe "treeCursorDemoteElemUnder" $ do
    it "produces valids on valids" $
      producesValidsOnValids3 $ treeCursorDemoteElemUnder @Rational @Rational
    it "Works on the example from the docs" $
      forAllValid $ \b1 ->
        forAllValid $ \b2 ->
          let demoteStart =
                TreeCursor
                  { treeAbove =
                      Just
                        TreeAbove
                          { treeAboveLefts = S.empty
                          , treeAboveAbove = Nothing
                          , treeAboveNode = 'p'
                          , treeAboveRights = S.empty
                          }
                  , treeCurrent = 'a'
                  , treeBelow = closedForest [Node 'b' []]
                  }
              demoteEnd =
                TreeCursor
                  { treeAbove =
                      Just
                        TreeAbove
                          { treeAboveLefts = S.empty
                          , treeAboveAbove =
                              Just
                                TreeAbove
                                  { treeAboveLefts = S.empty
                                  , treeAboveAbove = Nothing
                                  , treeAboveNode = 'p'
                                  , treeAboveRights = S.singleton $ node b2 [node 'b' []]
                                  }
                          , treeAboveNode = b1
                          , treeAboveRights = S.empty
                          }
                  , treeCurrent = 'a'
                  , treeBelow = emptyCForest
                  }
           in case treeCursorDemoteElemUnder b1 b2 demoteStart of
                Just tc' -> tc' `treeShouldBe` demoteEnd
                _ -> expectationFailure "treeCursorDemoteElemUnder should not have failed"
    it "demotes the current node to the level of its children" pending
  describe "treeCursorDemoteSubTreeUnder" $ do
    it "produces valids on valids" $
      producesValidsOnValids2 $ treeCursorDemoteSubTreeUnder @Rational @Rational
    it "Works on the example from the docs" $
      forAllValid $ \v -> do
        let demoteStart =
              TreeCursor
                {treeAbove = Nothing, treeCurrent = 'a', treeBelow = closedForest [Node 'b' []]}
            demoteEnd =
              TreeCursor
                { treeAbove =
                    Just
                      TreeAbove
                        { treeAboveLefts = S.empty
                        , treeAboveAbove = Nothing
                        , treeAboveNode = v
                        , treeAboveRights = S.empty
                        }
                , treeCurrent = 'a'
                , treeBelow = closedForest [Node 'b' []]
                }
        treeCursorDemoteSubTreeUnder v demoteStart `treeShouldBe` demoteEnd
    it "demotes the current subtree to the level of its children, by adding a root" pending
