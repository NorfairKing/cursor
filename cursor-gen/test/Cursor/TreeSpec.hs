{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.TreeSpec
    ( spec
    ) where

import Test.Hspec

--import Test.QuickCheck
import Test.Validity

import Cursor.Tree
import Cursor.Tree.Gen ()

spec :: Spec
spec = do
    eqSpec @(TreeCursor Int)
    functorSpec @TreeCursor
    genValidSpec @(TreeCursor Double)
    describe "makeNonEmptyCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeTreeCursor @Double)
    describe "singletonTreeCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (singletonTreeCursor @Double)
    describe "rebuildTreeCursor" $ do
        it "produces valid nonempty lists" $
            producesValidsOnValids (rebuildTreeCursor @Double)
        it "is the inverse of makeTreeCursor for integers" $
            inverseFunctions (makeTreeCursor @Int) rebuildTreeCursor
-- isMovementM :: (forall a. TreeCursor a -> Maybe (TreeCursor a)) -> Property
-- isMovementM func =
--     forAllValid $ \lec ->
--         case func (lec :: TreeCursor Int) of
--             Nothing -> pure () -- Fine
--             Just lec' ->
--                 let ne = rebuildTreeCursor lec
--                     ne' = rebuildTreeCursor lec'
--                  in unless (ne == ne') $
--                     expectationFailure $
--                     unlines
--                         [ "Cursor before:\n" ++ show lec
--                         , "Tree before:  \n" ++ show ne
--                         , "Cursor after: \n" ++ show lec'
--                         , "Tree after:   \n" ++ show ne'
--                         ]
--
-- isMovement :: (forall a. TreeCursor a -> TreeCursor a) -> Property
-- isMovement func =
--     forAllValid $ \lec ->
--         rebuildTreeCursor (lec :: TreeCursor Int) `shouldBe`
--         rebuildTreeCursor (func lec)
