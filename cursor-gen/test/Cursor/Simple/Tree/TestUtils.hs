{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Simple.Tree.TestUtils where

import Control.Monad (unless)
import Cursor.Simple.Tree hiding (TreeCursor)
import qualified Cursor.Simple.Tree as STC (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree (closedForest)
import Data.Tree
import Test.Hspec
import Test.QuickCheck
import Test.Validity

testMovement :: (forall a. STC.TreeCursor a -> STC.TreeCursor a) -> Spec
testMovement func = do
  it "produces valids on valids" $ producesValidsOnValids $ func @Bool
  it "is a movement" $ isMovement func

testMovementM :: (forall a. STC.TreeCursor a -> Maybe (STC.TreeCursor a)) -> Spec
testMovementM func = do
  it "produces valids on valids" $ producesValidsOnValids $ func @Bool
  it "is a movement" $ isMovementM func

isMovementM :: (forall a. STC.TreeCursor a -> Maybe (STC.TreeCursor a)) -> Property
isMovementM func =
  forAllValid @(STC.TreeCursor Int) $ \lec ->
    case func lec of
      Nothing -> pure () -- Fine
      Just lec' ->
        let ne = rebuildCTree $ rebuildTreeCursor lec
            ne' = rebuildCTree $ rebuildTreeCursor lec'
         in unless (ne == ne')
              $ expectationFailure
              $ unlines
                [ "Cursor before:\n" ++ drawTreeCursor lec,
                  "Tree before:  \n" ++ drawTree (fmap show ne),
                  "Cursor after: \n" ++ drawTreeCursor lec',
                  "Tree after:   \n" ++ drawTree (fmap show ne')
                ]

isMovement :: (forall a. STC.TreeCursor a -> STC.TreeCursor a) -> Property
isMovement func =
  forAllValid $ \lec ->
    rebuildTreeCursor (lec :: STC.TreeCursor Int) `shouldBe` rebuildTreeCursor (func lec)

treeShouldBe :: (Show a, Eq a) => STC.TreeCursor a -> STC.TreeCursor a -> Expectation
treeShouldBe actual expected =
  unless (actual == expected)
    $ expectationFailure
    $ unlines
      [ "The following should have been equal.",
        "actual:",
        drawTreeCursor actual,
        "expected:",
        drawTreeCursor expected
      ]

instance CanFail SwapResult where
  hasFailed (Swapped _) = False
  hasFailed _ = True
  resultIfSucceeded (Swapped a) = Just a
  resultIfSucceeded _ = Nothing

node :: a -> [CTree a] -> CTree a
node a ts = CNode a $ closedForest $ map rebuildCTree ts
