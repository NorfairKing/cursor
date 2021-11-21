{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.Simple.MapSpec
  ( spec,
  )
where

import Control.Monad
import Cursor.Simple.Map
import Cursor.Simple.Map.Gen ()
import Lens.Micro
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  describe "makeMapCursor" $
    it "produces valid cursors" $
      producesValid (makeMapCursor @Bool @Bool)
  describe "makeMapCursorWithSelection" $
    it "produces valid cursors" $
      producesValid2 (makeMapCursorWithSelection @Bool @Bool)
  describe "singletonMapCursorKey" $
    it "produces valid cursors" $
      producesValid2 (singletonMapCursorKey @Bool @Bool @Bool @Bool)
  describe "singletonMapCursorValue" $
    it "produces valid cursors" $
      producesValid2 (singletonMapCursorValue @Bool @Bool @Bool @Bool)
  describe "rebuildMapCursor" $ do
    it "produces valid Nonempty lists" $ producesValid (rebuildMapCursor @Bool @Bool)
    it "is the inverse of makeMapCursor for integers" $
      inverseFunctions (makeMapCursor @Bool @Bool) rebuildMapCursor
  describe "mapCursorNonEmptyCursorL" $
    lensSpec (mapCursorNonEmptyCursorL @Bool @Bool @Bool @Bool)
  describe "mapCursorElemL" $ lensSpec (mapCursorElemL @Bool @Bool @Bool @Bool)
  describe "mapCursorSelectKey" $
    it "produces valid cursors" $
      producesValid (mapCursorSelectKey @Bool @Bool)
  describe "mapCursorSelectValue" $
    it "produces valid cursors" $
      producesValid (mapCursorSelectValue @Bool @Bool)
  describe "mapCursorToggleSelected" $
    it "produces valid cursors" $
      producesValid (mapCursorToggleSelected @Bool @Bool)
  describe "mapCursorSelectPrev" $ do
    it "produces valid cursors" $ producesValid (mapCursorSelectPrev @Bool @Bool)
    it "is a movement" $ isMovementM mapCursorSelectPrev
    it "selects the previous element" pending
  describe "mapCursorSelectNext" $ do
    it "produces valid cursors" $ producesValid (mapCursorSelectNext @Bool @Bool)
    it "is a movement" $ isMovementM mapCursorSelectNext
    it "selects the next element" pending
  describe "mapCursorSelectFirst" $ do
    it "produces valid cursors" $ producesValid (mapCursorSelectFirst @Bool @Bool)
    it "is a movement" $ isMovement mapCursorSelectFirst
    it "is idempotent" $ idempotent (mapCursorSelectFirst @Bool @Bool)
    it "selects the first element" pending
  describe "mapCursorSelectLast" $ do
    it "produces valid cursors" $ producesValid (mapCursorSelectLast @Bool @Bool)
    it "is a movement" $ isMovement mapCursorSelectLast
    it "is idempotent" $ idempotent (mapCursorSelectLast @Bool @Bool)
    it "selects the last element" pending
  describe "mapCursorSelection" $ do
    it "produces valid ints" $ producesValid (mapCursorSelection @Bool @Bool @Bool @Bool)
    it "returns the index of the currently selected element" pending
  describe "mapCursorSelectIndex" $ do
    it "produces valid cursors" $ producesValid2 (mapCursorSelectIndex @Bool @Bool)
    it "is the identity function when given the current selection" $
      forAllValid $
        \nec ->
          mapCursorSelectIndex (mapCursorSelection nec) nec
            `shouldBe` Just (nec :: MapCursor Bool Bool)
    it "returns selects the element at the given index" pending
  describe "mapCursorInsert" $ do
    it "produces valid cursors" $ producesValid3 (mapCursorInsert @Bool @Bool @Bool @Bool)
    it "inserts a character before the cursor" pending
  describe "mapCursorAppend" $ do
    it "produces valid cursors" $ producesValid3 (mapCursorAppend @Bool @Bool @Bool @Bool)
    it "inserts a character after the cursor" pending
  describe "mapCursorInsertAndSelectKey" $
    it "produces valid cursors" $
      producesValid3 (mapCursorInsertAndSelectKey @Bool @Bool)
  describe "mapCursorAppendAndSelectKey" $
    it "produces valid cursors" $
      producesValid3 (mapCursorAppendAndSelectKey @Bool @Bool)
  describe "mapCursorInsertAndSelectValue" $
    it "produces valid cursors" $
      producesValid3 (mapCursorInsertAndSelectValue @Bool @Bool)
  describe "mapCursorAppendAndSelectValue" $
    it "produces valid cursors" $
      producesValid3 (mapCursorAppendAndSelectValue @Bool @Bool)
  describe "mapCursorRemoveElem" $ do
    it "produces valid cursors" $ producesValid (mapCursorRemoveElem @Bool @Bool)
    it "removes an element" pending
  describe "mapCursorDeleteElem" $ do
    it "produces valid cursors" $ producesValid (mapCursorDeleteElem @Bool @Bool)
    it "deletes an element" pending
  describe "mapCursorSearch" $ do
    it "produces valid cursors when looking for an equal pair" $
      forAllValid $
        \(k, v) ->
          producesValid $ mapCursorSearch @Bool @Bool (\k_ v_ -> k_ == k && v_ == v)
    it "is indeed the right value when it finds a value and is looking for an equal element" $
      forAllValid $
        \(k, v) ->
          forAllValid $ \nec ->
            case mapCursorSearch (\k_ v_ -> k_ == k && v_ == v) nec of
              Nothing -> pure ()
              Just e -> rebuildKeyValueCursor (e ^. mapCursorElemL) `shouldBe` (k :: Bool, v :: Bool)
  describe "mapCursorSelectOrAdd" $
    it "produces valid cursors when looking for an equal element" $
      forAllValid $
        \(k, v) ->
          producesValid $
            mapCursorSelectOrAdd
              (\k_ v_ -> k_ == k && v_ == v)
              (makeKeyValueCursorKey (k :: Bool) (v :: Bool))

isMovementM :: (forall k v. MapCursor k v -> Maybe (MapCursor k v)) -> Property
isMovementM func =
  forAllValid $ \lec ->
    case func (lec :: MapCursor Bool Bool) of
      Nothing -> pure () -- Fine
      Just lec' ->
        let ne = rebuildMapCursor lec
            ne' = rebuildMapCursor lec'
         in unless (ne == ne') $
              expectationFailure $
                unlines
                  [ "Cursor before:\n" ++ show lec,
                    "Map before:  \n" ++ show ne,
                    "Cursor after: \n" ++ show lec',
                    "Map after:   \n" ++ show ne'
                  ]

isMovement :: (forall k v. MapCursor k v -> MapCursor k v) -> Property
isMovement func =
  forAllValid $ \lec ->
    rebuildMapCursor (lec :: MapCursor Bool Bool) `shouldBe` rebuildMapCursor (func lec)
