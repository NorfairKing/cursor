{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.MapSpec
    ( spec
    ) where

import Test.Hspec

-- import Test.QuickCheck
import Test.Validity

-- import Control.Monad
import Cursor.Map
import Cursor.Map.Gen ()

spec :: Spec
spec = do
    eqSpec @(MapCursor Int Int)
    functorSpec @(MapCursor Int)
    genValidSpec @(MapCursor Double Double)
    eqSpec @(KeyValueCursor Int Int)
    functorSpec @(KeyValueCursor Int)
    genValidSpec @(KeyValueCursor Double Double)
    describe "makeMapCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeMapCursor @Double @Double)
    describe "makeMapCursorWithSelection" $
        it "produces valid cursors" $
        producesValidsOnValids2 (makeMapCursorWithSelection @Double @Double)
    describe "makeKeyValueCursor" $
        it "produces valid cursors" $
        producesValidsOnValids2 (makeKeyValueCursor @Double @Double)
    describe "singletonKeyValueCursor" $
        it "produces valid cursors" $
        producesValidsOnValids2 (singletonKeyValueCursor @Double @Double)
    describe "rebuildMapCursor" $ do
        it "produces valid Nonempty lists" $
            producesValidsOnValids (rebuildMapCursor @Double @Double)
        it "is the inverse of makeMapCursor for integers" $
            inverseFunctions (makeMapCursor @Int @Int) rebuildMapCursor
-- isMovementM :: (forall k v. MapCursor k v -> Maybe (MapCursor k v)) -> Property
-- isMovementM func =
--     forAllValid $ \lec ->
--         case func (lec :: MapCursor Double Double) of
--             Nothing -> pure () -- Fine
--             Just lec' ->
--                 let ne = rebuildMapCursor lec
--                     ne' = rebuildMapCursor lec'
--                  in unless (ne == ne') $
--                     expectationFailure $
--                     unlines
--                         [ "Cursor before:\n" ++ show lec
--                         , "Map before:  \n" ++ show ne
--                         , "Cursor after: \n" ++ show lec'
--                         , "Map after:   \n" ++ show ne'
--                         ]
--
-- isMovement :: (forall k v. MapCursor k v -> MapCursor k v) -> Property
-- isMovement func =
--     forAllValid $ \lec ->
--         rebuildMapCursor (lec :: MapCursor Int Int) `shouldBe`
--         rebuildMapCursor (func lec)
