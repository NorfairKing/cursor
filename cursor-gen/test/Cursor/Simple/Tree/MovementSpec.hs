{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.Tree.MovementSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Control.Monad (unless)

import qualified Data.Sequence as S
import Text.Show.Pretty

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree (CTree(..), TreeAbove(..), TreeCursor(..), emptyCForest, openForest)

import Cursor.Simple.Tree.TestUtils

spec :: Spec
spec = do
  describe "treeCursorSelection" $
    it "produces valids on valids" $
    producesValidsOnValids (treeCursorSelection @Rational @Rational)
  describe "treeCursorSelect" $ do
    it "produces valids on valids" $ producesValidsOnValids2 (treeCursorSelect @Rational)
    it "is identity with the current selection" $
      forAllValid $ \tc ->
        let sel = treeCursorSelection tc
         in case treeCursorSelect @Rational sel tc of
              Nothing -> expectationFailure "treeCursorSelect should not have failed."
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
  describe "treeCursorSelectPrevOnSameLevel" $ do
    testMovementM treeCursorSelectPrevOnSameLevel
    it "selects the previous element" pending
    it "after treeCursorSelectNextOnSameLevel is identity if they don't fail" $ do
      inverseFunctionsIfSucceedOnValid
        (treeCursorSelectNextOnSameLevel @Rational)
        (treeCursorSelectPrevOnSameLevel @Rational)
  describe "treeCursorSelectNextOnSameLevel" $ do
    testMovementM treeCursorSelectNextOnSameLevel
    it "selects the next element" pending
    it "after treeCursorSelectPrevOnSameLevel is identity if they don't fail" $ do
      inverseFunctionsIfSucceedOnValid
        (treeCursorSelectPrevOnSameLevel @Rational)
        (treeCursorSelectNextOnSameLevel @Rational)
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
                       { treeAboveLefts =
                           S.singleton $
                           CNode 1 $ openForest [CNode 2 $ openForest [CNode 3 emptyCForest]]
                       , treeAboveAbove = Nothing
                       , treeAboveNode = 0
                       , treeAboveRights = S.empty
                       })
              , treeCurrent = 4 :: Int
              , treeBelow = emptyCForest
              }
          expected =
            TreeCursor
              { treeAbove =
                  Just
                    (TreeAbove
                       { treeAboveLefts = S.empty
                       , treeAboveAbove =
                           Just
                             (TreeAbove
                                { treeAboveLefts = S.empty
                                , treeAboveAbove =
                                    Just
                                      (TreeAbove
                                         { treeAboveLefts = S.empty
                                         , treeAboveAbove = Nothing
                                         , treeAboveNode = 0
                                         , treeAboveRights = S.singleton $ CNode 4 emptyCForest
                                         })
                                , treeAboveNode = 1
                                , treeAboveRights = S.empty
                                })
                       , treeAboveNode = 2
                       , treeAboveRights = S.empty
                       })
              , treeCurrent = 3
              , treeBelow = emptyCForest
              }
      case treeCursorSelectAbovePrev start of
        Nothing -> expectationFailure "treeCursorSelectAbovePrev should not have failed"
        Just r -> r `treeShouldBe` expected
    it "selects the previous element" pending
    it "after treeCursorSelectAboveNext is identity if they don't fail" $ do
      forAllValid $ \tc ->
        case treeCursorSelectAboveNext @Rational tc of
          Nothing -> pure ()
          Just tc' ->
            case treeCursorSelectAbovePrev tc' of
              Nothing -> expectationFailure "treeCursorSelectAbovePrev should not have failed."
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
                       { treeAboveLefts = S.empty
                       , treeAboveAbove =
                           Just
                             (TreeAbove
                                { treeAboveLefts = S.empty
                                , treeAboveAbove =
                                    Just
                                      (TreeAbove
                                         { treeAboveLefts = S.empty
                                         , treeAboveAbove = Nothing
                                         , treeAboveNode = 0
                                         , treeAboveRights = S.singleton $ node 4 []
                                         })
                                , treeAboveNode = 1
                                , treeAboveRights = S.empty
                                })
                       , treeAboveNode = 2
                       , treeAboveRights = S.empty
                       })
              , treeCurrent = 3
              , treeBelow = emptyCForest
              }
          expected =
            TreeCursor
              { treeAbove =
                  Just
                    (TreeAbove
                       { treeAboveLefts =
                           S.singleton $
                           CNode 1 $ openForest [CNode 2 $ openForest [CNode 3 emptyCForest]]
                       , treeAboveAbove = Nothing
                       , treeAboveNode = 0
                       , treeAboveRights = S.empty
                       })
              , treeCurrent = 4 :: Int
              , treeBelow = emptyCForest
              }
      case treeCursorSelectAboveNext start of
        Nothing -> expectationFailure "treeCursorSelectAboveNext should not have failed."
        Just r -> r `treeShouldBe` expected
    it "selects the next element" pending
    it "after treeCursorSelectAbovePrev is identity if they don't fail" $ do
      forAllValid $ \tc ->
        case treeCursorSelectAbovePrev @Rational tc of
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
  describe "treeCursorSelectPrev" $ do
    testMovementM treeCursorSelectPrev
    it "selects the previous element" pending
    it "after treeCursorSelectNext is identity if they don't fail" $ do
      inverseFunctionsIfSucceedOnValid
        (treeCursorSelectNext @Rational)
        (treeCursorSelectPrev @Rational)
  describe "treeCursorSelectNext" $ do
    testMovementM treeCursorSelectNext
    it "selects the next element" pending
    it "after treeCursorSelectPrev is identity if they don't fail" $ do
      inverseFunctionsIfSucceedOnValid
        (treeCursorSelectPrev @Rational)
        (treeCursorSelectNext @Rational)
  describe "treeCursorSelectFirst" $ do
    testMovement treeCursorSelectFirst
    it "selects the first element" pending
    it "is idempotent" $ idempotentOnValid $ treeCursorSelectFirst @Rational
  describe "treeCursorSelectLast" $ do
    testMovement treeCursorSelectLast
    it "selects the last element" pending
    it "is idempotent" $ idempotentOnValid $ treeCursorSelectLast @Rational
  describe "treeCursorSelectAbove" $ do
    testMovementM treeCursorSelectAbove
    it "selects the element above" pending
    it "after treeCursorSelectBelow is identity if they don't fail" $ do
      inverseFunctionsIfSucceedOnValid (treeCursorSelectBelowAtStart @Rational) $
        treeCursorSelectAbove @Rational
  describe "treeCursorSelectBelowAtPos" $ do
    it "produces valids on valids" $ producesValidsOnValids2 $ treeCursorSelectBelowAtPos @Rational
    it "is a movement" $ forAllValid $ \n -> isMovementM $ treeCursorSelectBelowAtPos n
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
