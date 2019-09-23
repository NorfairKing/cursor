{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.TextSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Control.Monad

import Cursor.List
import Cursor.Text
import Cursor.Text.Gen ()

spec :: Spec
spec = do
  eqSpec @TextCursor
  genValidSpec @TextCursor
  describe "Validity TextCursor" $ do
    it "considers a text cursor with a newline in the previous characters invalid" $
      shouldBeInvalid $
      TextCursor {textCursorList = ListCursor {listCursorPrev = "\n", listCursorNext = ""}}
    it "considers a text cursor with a newline in the next characters invalid" $
      shouldBeInvalid $
      TextCursor {textCursorList = ListCursor {listCursorPrev = "", listCursorNext = "\n"}}
    it "considers a text cursor with an unsafe character in the previous characters invalid" $
      shouldBeInvalid $
      TextCursor {textCursorList = ListCursor {listCursorPrev = "\55810", listCursorNext = ""}}
    it "considers a text cursor with an unsafe character in the next characters invalid" $
      shouldBeInvalid $
      TextCursor {textCursorList = ListCursor {listCursorPrev = "\55810", listCursorNext = "\n"}}
  describe "emptyTextCursor" $ it "is valid" $ shouldBeValid emptyTextCursor
  describe "makeTextCursor" $
    it "produces valid list cursors" $ producesValidsOnValids makeTextCursor
  describe "makeTextCursorWithSelection" $
    it "produces valid list cursors" $ producesValidsOnValids2 makeTextCursorWithSelection
  describe "rebuildTextCursor" $ do
    it "produces valid lists" $ producesValidsOnValids rebuildTextCursor
    it "is the inverse of makeTextCursor" $
      inverseFunctionsIfFirstSucceedsOnValid makeTextCursor rebuildTextCursor
    it "is the inverse of makeTextCursorWithSelection for any index" $
      forAllUnchecked $ \i ->
        inverseFunctionsIfFirstSucceedsOnValid (makeTextCursorWithSelection i) rebuildTextCursor
  describe "textCursorNull" $ it "produces valid bools" $ producesValidsOnValids textCursorNull
  describe "textCursorLength" $ it "produces valid ints" $ producesValidsOnValids textCursorLength
  describe "textCursorIndex" $ it "produces valid indices" $ producesValidsOnValids textCursorIndex
  describe "textCursorSelectPrev" $ do
    it "produces valid cursors" $ producesValidsOnValids textCursorSelectPrev
    it "is a movement" $ isMovementM textCursorSelectPrev
    it "selects the previous position" pending
  describe "textCursorSelectNext" $ do
    it "produces valid cursors" $ producesValidsOnValids textCursorSelectNext
    it "is a movement" $ isMovementM textCursorSelectNext
    it "selects the next position" pending
  describe "textCursorSelectIndex" $ do
    it "produces valid cursors" $ producesValidsOnValids2 textCursorSelectIndex
    it "is a movement" $ forAllUnchecked $ \ix -> isMovement (textCursorSelectIndex ix)
    it "selects the position at the given index" pending
    it "produces a cursor that has the given selection for valid selections in the cursor" $
      forAllValid $ \tc ->
        forAll (choose (0, textCursorLength tc)) $ \i ->
          textCursorIndex (textCursorSelectIndex i tc) `shouldBe` i
  describe "textCursorSelectStart" $ do
    it "produces valid cursors" $ producesValidsOnValids textCursorSelectStart
    it "is a movement" $ isMovement textCursorSelectStart
    it "is idempotent" $ idempotent textCursorSelectStart
    it "selects the starting position" pending
  describe "textCursorSelectEnd" $ do
    it "produces valid cursors" $ producesValidsOnValids textCursorSelectEnd
    it "is a movement" $ isMovement textCursorSelectEnd
    it "is idempotent" $ idempotent textCursorSelectEnd
    it "selects the end position" pending
  describe "textCursorPrevChar" $ do
    it "produces valid items" $ producesValidsOnValids textCursorPrevChar
    it "returns the item before the position" pending
  describe "textCursorNextChar" $ do
    it "produces valid items" $ producesValidsOnValids textCursorNextChar
    it "returns the item after the position" pending
  describe "textCursorInsert" $ do
    it "produces valids" $ forAllValid $ \d -> producesValidsOnValids (textCursorInsert d)
    it "inserts an item before the cursor" $ pending
  describe "textCursorAppend" $ do
    it "produces valids" $ forAllValid $ \d -> producesValidsOnValids (textCursorAppend d)
    it "inserts an item after the cursor" $ pending
  describe "textCursorRemove" $ do
    it "produces valids" $ validIfSucceedsOnValid textCursorRemove
    it "removes an item before the cursor" $ pending
  describe "textCursorDelete" $ do
    it "produces valids" $ validIfSucceedsOnValid textCursorDelete
    it "removes an item before the cursor" $ pending
  describe "textCursorSplit" $ do
    it "produces valids" $ producesValidsOnValids textCursorSplit
    it "produces two list cursors that rebuild to the rebuilding of the original" $
      forAllValid $ \lc ->
        let (lc1, lc2) = textCursorSplit lc
         in (rebuildTextCursor lc1 <> rebuildTextCursor lc2) `shouldBe` rebuildTextCursor lc
  describe "textCursorCombine" $ do
    it "produces valids" $ producesValidsOnValids2 textCursorCombine
    it "produces a list that rebuilds to the rebuilding of the original two cursors" $
      forAllValid $ \lc1 ->
        forAllValid $ \lc2 ->
          let lc = textCursorCombine lc1 lc2
           in rebuildTextCursor lc `shouldBe` (rebuildTextCursor lc1 <> rebuildTextCursor lc2)

isMovementM :: (TextCursor -> Maybe TextCursor) -> Property
isMovementM func =
  forAllValid $ \tc ->
    case func tc of
      Nothing -> pure () -- Fine
      Just tc' ->
        let t = rebuildTextCursor tc
            t' = rebuildTextCursor tc'
         in unless (t == t') $
            expectationFailure $
            unlines
              [ "Cursor before:\n" ++ show tc
              , "Text before:  \n" ++ show t
              , "Cursor after: \n" ++ show tc'
              , "Text after:   \n" ++ show t'
              ]

isMovement :: (TextCursor -> TextCursor) -> Property
isMovement func =
  forAllValid $ \lec -> rebuildTextCursor lec `shouldBe` rebuildTextCursor (func lec)
