{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TextFieldSpec
  ( spec,
  )
where

import Control.Monad
import Cursor.List.NonEmpty
import Cursor.TextField
import Cursor.TextField.Gen ()
import Cursor.Types
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
  eqSpec @TextFieldCursor
  genValidSpec @TextFieldCursor
  describe "Validity TextFieldCursor" $ do
    it "consider a textfield with a newline in the previous lines invalid" $
      forAllValid $
        \tc ->
          shouldBeInvalid $
            TextFieldCursor
              { textFieldCursorNonEmpty =
                  NonEmptyCursor
                    { nonEmptyCursorPrev = ["\n"],
                      nonEmptyCursorCurrent = tc,
                      nonEmptyCursorNext = []
                    }
              }
    it "consider a textfield with a newline in the next lines invalid" $
      forAllValid $
        \tc ->
          shouldBeInvalid $
            TextFieldCursor
              { textFieldCursorNonEmpty =
                  NonEmptyCursor
                    { nonEmptyCursorPrev = [],
                      nonEmptyCursorCurrent = tc,
                      nonEmptyCursorNext = ["\n"]
                    }
              }
  describe "makeTextFieldCursor" $ do
    it "produces a valid cursor for \"\\n\"" $ shouldBeValid $ makeTextFieldCursor "\n"
    it "produces a valid cursor for \"\\n\\n\"" $ shouldBeValid $ makeTextFieldCursor "\n\n"
    it "produces valid cursors" $ producesValid makeTextFieldCursor
  describe "makeTextFieldCursorWithSelection" $ do
    it "produces a valid cursor for \"\\n\"" $
      forAllValid $
        \x ->
          forAllValid $ \y -> shouldBeValid $ makeTextFieldCursorWithSelection x y "\n"
    it "produces a valid cursor for \"\\n\\n\"" $
      forAllValid $
        \x ->
          forAllValid $ \y -> shouldBeValid $ makeTextFieldCursorWithSelection x y "\n\n"
    it "produces valid cursors" $ producesValid3 makeTextFieldCursorWithSelection
    it "is the inverse of rebuildTextFieldCursor when using the current selection" $
      forAllValid $
        \tfc -> do
          let (x, y) = textFieldCursorSelection tfc
              t = rebuildTextFieldCursor tfc
          case makeTextFieldCursorWithSelection x y t of
            Nothing -> expectationFailure "makeTextFieldCursorWithSelection should not have failed."
            Just tfc' ->
              unless (tfc' == tfc) $
                expectationFailure $
                  unlines
                    [ "expected",
                      ppShow tfc,
                      "actual",
                      ppShow tfc',
                      "The selection of the original (expected) cursor was:",
                      show (x, y),
                      "The rebuild text was:",
                      show t
                    ]
  describe "rebuildTextFieldCursorLines" $ do
    it "produces valid lists" $ producesValid rebuildTextFieldCursorLines
    it "produces texts without newlines" $
      forAllValid $
        \tfc -> do
          let ls = NE.toList $ rebuildTextFieldCursorLines tfc
          unless (all (T.all (/= '\n')) ls) $
            expectationFailure $
              unlines $
                "Some of the following lines contain a newline:" : map show ls
  describe "rebuildTextFieldCursor" $ do
    it "produces valid texts" $ producesValid rebuildTextFieldCursor
    it "is the inverse of makeTextFieldCursor" $
      inverseFunctions makeTextFieldCursor rebuildTextFieldCursor
    it "is the inverse of makeTextFieldCursorWithSelection for integers, for any index" $
      forAllValid $
        \x ->
          forAllValid $ \y ->
            inverseFunctionsIfFirstSucceeds
              (makeTextFieldCursorWithSelection x y)
              rebuildTextFieldCursor
  describe "textFieldCursorSelection" $
    it "produces valid tuples" $
      producesValid textFieldCursorSelection
  describe "emptyTextFieldCursor" $ it "is valid" $ shouldBeValid emptyTextFieldCursor
  describe "nullTextFieldCursor" $ it "produces valid" $ producesValid nullTextFieldCursor
  describe "textFieldCursorSelectPrevLine" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectPrevLine
    it "is a movement" $ isMovementM textFieldCursorSelectPrevLine
    it "selects the previous line" pending
  describe "textFieldCursorSelectNextLine" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectNextLine
    it "is a movement" $ isMovementM textFieldCursorSelectNextLine
    it "selects the next line" pending
  describe "textFieldCursorSelectFirstLine" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectFirstLine
    it "is a movement" $ isMovement textFieldCursorSelectFirstLine
    it "is idempotent" $ idempotent textFieldCursorSelectFirstLine
    it "selects the first line" pending
  describe "textFieldCursorSelectLastLine" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectLastLine
    it "is a movement" $ isMovement textFieldCursorSelectLastLine
    it "is idempotent" $ idempotent textFieldCursorSelectLastLine
    it "selects the last line" pending
  describe "textFieldCursorSelectPrevChar" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectPrevChar
    it "selects the previous character on the current line" pending
  describe "textFieldCursorSelectNextChar" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectNextChar
    it "selects the previous character on the current line" pending
  describe "textFieldCursorSelectBeginWord" $
    it "produces valid cursors" $
      producesValid textFieldCursorSelectBeginWord
  describe "textFieldCursorSelectEndWord" $
    it "produces valid cursors" $
      producesValid textFieldCursorSelectEndWord
  describe "textFieldCursorSelectPrevWord" $
    it "produces valid cursors" $
      producesValid textFieldCursorSelectPrevWord
  describe "textFieldCursorSelectNextWord" $
    it "produces valid cursors" $
      producesValid textFieldCursorSelectNextWord
  describe "textFieldCursorIndexOnLine" $ do
    it "produces valid indices" $ producesValid textFieldCursorIndexOnLine
    it "returns the index on the current line" pending
  describe "textFieldCursorSelectIndexOnLine" $ do
    it "produces valid cursors for any index" $
      producesValid2 textFieldCursorSelectIndexOnLine
    it "selects the given index on the current line" pending
  describe "textFieldCursorInsertChar" $ do
    it "produces valid cursors" $
      forAllValid $
        \d -> producesValid (textFieldCursorInsertChar d)
    it "inserts a character before the cursor on the current line" pending
  describe "textFieldCursorAppendChar" $ do
    it "produces valid cursors" $
      forAllValid $
        \d -> producesValid (textFieldCursorAppendChar d)
    it "inserts a character after the cursor on the currrent line" pending
  describe "textFieldCursorInsertNewline" $ do
    it "produces valid cursors" $ producesValid textFieldCursorInsertNewline
    it "inserts a new line" pending
  describe "textFieldCursorAppendNewline" $
    it "produces valid cursors" $
      producesValid textFieldCursorAppendNewline
  describe "textFieldCursorRemove" $ do
    it "produces valid cursors" $ producesValid textFieldCursorRemove
    it "removes empty text field cursor" $
      textFieldCursorRemove emptyTextFieldCursor `shouldBe` Just Deleted
    it "removes a character or a line" pending
  describe "textFieldCursorDelete" $ do
    it "produces valid cursors" $ producesValid textFieldCursorDelete
    it "removes empty text field cursor" $
      textFieldCursorDelete emptyTextFieldCursor `shouldBe` Just Deleted
    it "deletes a character or a line" pending
  describe "textFieldCursorSelectStartOfLine" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectStartOfLine
    it "selects the start of the current line" pending
  describe "textFieldCursorSelectEndOfLine" $ do
    it "produces valid cursors" $ producesValid textFieldCursorSelectEndOfLine
    it "selects the end of the current line" pending

isMovementM :: (TextFieldCursor -> Maybe TextFieldCursor) -> Property
isMovementM func =
  forAllValid $ \tfc ->
    case func tfc of
      Nothing -> pure () -- Fine
      Just tfc' ->
        let tf = rebuildTextFieldCursor tfc
            tf' = rebuildTextFieldCursor tfc'
         in unless (tf == tf') $
              expectationFailure $
                unlines
                  [ "Cursor before:\n" ++ show tfc,
                    "TextField before:  \n" ++ show tf,
                    "Cursor after: \n" ++ show tfc',
                    "TextField after:   \n" ++ show tf'
                  ]

isMovement :: (TextFieldCursor -> TextFieldCursor) -> Property
isMovement func =
  forAllValid $ \tfc -> rebuildTextFieldCursor tfc `shouldBe` rebuildTextFieldCursor (func tfc)
