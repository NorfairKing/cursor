{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.ListSpec
  ( spec,
  )
where

import Control.Monad
import Cursor.List
import Cursor.List.Gen ()
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  eqSpec @(ListCursor Bool)
  functorSpec @ListCursor
  genValidSpec @(ListCursor Bool)
  describe "emptyListCursor" $ it "is valid" $ shouldBeValid (emptyListCursor @Bool)
  describe "makeListCursor"
    $ it "produces valid list cursors"
    $ producesValidsOnValids (makeListCursor @Bool)
  describe "makeListCursorWithSelection"
    $ it "produces valid list cursors"
    $ producesValidsOnValids2 (makeListCursorWithSelection @Bool)
  describe "rebuildListCursor" $ do
    it "produces valid lists" $ producesValidsOnValids (rebuildListCursor @Bool)
    it "is the inverse of makeListCursor" $
      inverseFunctions (makeListCursor @Bool) rebuildListCursor
    it "is the inverse of makeListCursorWithSelection for any index"
      $ forAllUnchecked
      $ \i ->
        inverseFunctionsIfFirstSucceeds (makeListCursorWithSelection @Bool i) rebuildListCursor
  describe "listCursorNull"
    $ it "produces valid bools"
    $ producesValidsOnValids (listCursorNull @Bool)
  describe "listCursorLength"
    $ it "produces valid bools"
    $ producesValidsOnValids (listCursorLength @Bool)
  describe "listCursorIndex"
    $ it "produces valid indices"
    $ producesValidsOnValids (listCursorIndex @Bool)
  describe "listCursorSelectPrev" $ do
    it "produces valid cursors" $ producesValidsOnValids (listCursorSelectPrev @Bool)
    it "is a movement" $ isMovementM listCursorSelectPrev
    it "selects the previous position" pending
  describe "listCursorSelectNext" $ do
    it "produces valid cursors" $ producesValidsOnValids (listCursorSelectNext @Bool)
    it "is a movement" $ isMovementM listCursorSelectNext
    it "selects the next position" pending
  describe "listCursorSelectIndex" $ do
    it "produces valid cursors" $ producesValidsOnValids2 (listCursorSelectIndex @Bool)
    it "is a movement" $ forAllUnchecked $ \ix -> isMovement (listCursorSelectIndex ix)
    it "selects the position at the given index" pending
  describe "listCursorPrevItem" $ do
    it "produces valid items" $ producesValidsOnValids (listCursorPrevItem @Bool)
    it "returns the item before the position" pending
  describe "listCursorNextItem" $ do
    it "produces valid items" $ producesValidsOnValids (listCursorNextItem @Bool)
    it "returns the item after the position" pending
  describe "listCursorPrevUntil" $ do
    it "produces valid cursors" $ producesValidsOnValids (listCursorPrevUntil @Bool id)
    it "produces a cursor where the previous item either satisfies the predicate or is empty" $ forAllValid $ \cursor -> do
      let predicate = id
          result = listCursorPrevUntil @Bool predicate cursor
      case listCursorPrevItem result of
        Just item -> item `shouldSatisfy` predicate
        Nothing -> pure ()
  describe "listCursorNextUntil" $ do
    it "produces valid cursors" $ producesValidsOnValids (listCursorNextUntil @Bool id)
    it "produces a cursor where the previous item either satisfies the predicate or is empty" $ forAllValid $ \cursor -> do
      let predicate = id
          result = listCursorNextUntil @Bool predicate cursor
      case listCursorNextItem result of
        Just item -> item `shouldSatisfy` predicate
        Nothing -> pure ()
  describe "listCursorSelectStart" $ do
    it "produces valid cursors" $ producesValidsOnValids (listCursorSelectStart @Bool)
    it "is a movement" $ isMovement listCursorSelectStart
    it "is idempotent" $ idempotentOnValid (listCursorSelectStart @Bool)
    it "selects the starting position" pending
  describe "listCursorSelectEnd" $ do
    it "produces valid cursors" $ producesValidsOnValids (listCursorSelectEnd @Bool)
    it "is a movement" $ isMovement listCursorSelectEnd
    it "is idempotent" $ idempotentOnValid (listCursorSelectEnd @Bool)
    it "selects the end position" pending
  describe "listCursorInsert" $ do
    it "produces valids" $ forAllValid $ \d -> producesValidsOnValids (listCursorInsert @Bool d)
    it "inserts an item before the cursor" pending
  describe "listCursorAppend" $ do
    it "produces valids" $ forAllValid $ \d -> producesValidsOnValids (listCursorAppend @Bool d)
    it "inserts an item after the cursor" pending
  describe "listCursorInsertList"
    $ it "produces valids"
    $ forAllValid
    $ \d -> producesValidsOnValids (listCursorInsertList @Bool d)
  describe "listCursorAppendList"
    $ it "produces valids"
    $ forAllValid
    $ \d -> producesValidsOnValids (listCursorAppendList @Bool d)
  describe "listCursorRemove" $ do
    it "produces valids" $ validIfSucceedsOnValid (listCursorRemove @Bool)
    it "removes an item before the cursor" pending
  describe "listCursorDelete" $ do
    it "produces valids" $ validIfSucceedsOnValid (listCursorDelete @Bool)
    it "removes an item before the cursor" pending
  describe "listCursorSplit" $ do
    it "produces valids" $ producesValidsOnValids (listCursorSplit @Bool)
    it "produces two list cursors that rebuild to the rebuilding of the original"
      $ forAllValid
      $ \lc ->
        let (lc1, lc2) = listCursorSplit (lc :: ListCursor Bool)
         in (rebuildListCursor lc1 ++ rebuildListCursor lc2) `shouldBe` rebuildListCursor lc
  describe "listCursorCombine" $ do
    it "produces valids" $ producesValidsOnValids2 (listCursorCombine @Bool)
    it "produces a list that rebuilds to the rebuilding of the original two cursors"
      $ forAllValid
      $ \lc1 ->
        forAllValid $ \lc2 ->
          let lc = listCursorCombine lc1 (lc2 :: ListCursor Bool)
           in rebuildListCursor lc `shouldBe` (rebuildListCursor lc1 ++ rebuildListCursor lc2)

isMovementM :: (forall a. ListCursor a -> Maybe (ListCursor a)) -> Property
isMovementM func =
  forAllValid $ \lec ->
    case func (lec :: ListCursor Bool) of
      Nothing -> pure () -- Fine
      Just lec' ->
        let ne = rebuildListCursor lec
            ne' = rebuildListCursor lec'
         in unless (ne == ne')
              $ expectationFailure
              $ unlines
                [ "Cursor before:\n" ++ show lec,
                  "List before:  \n" ++ show ne,
                  "Cursor after: \n" ++ show lec',
                  "List after:   \n" ++ show ne'
                ]

isMovement :: (forall a. ListCursor a -> ListCursor a) -> Property
isMovement func =
  forAllValid $ \lec ->
    rebuildListCursor (lec :: ListCursor Bool) `shouldBe` rebuildListCursor (func lec)
