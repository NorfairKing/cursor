{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.Tree.PromoteSpec
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

import Cursor.Simple.Tree.TestUtils

spec :: Spec
spec = do
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
