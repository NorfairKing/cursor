{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TextFieldSpec
    ( spec
    ) where

import Test.Hspec

import Test.QuickCheck
import Test.Validity

-- import Test.Validity.Optics
import Text.Show.Pretty (ppShow)

import qualified Data.Text as T

import qualified Data.List.NonEmpty as NE

import Control.Monad

import Cursor.TextField
import Cursor.TextField.Gen ()
import Cursor.Types

spec :: Spec
spec = do
    eqSpec @TextFieldCursor
    genValidSpec @TextFieldCursor
    describe "makeTextFieldCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeTextFieldCursor
    describe "makeTextFieldCursorWithSelection" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 makeTextFieldCursorWithSelection
        it
            "is the inverse of rebuildTextFieldCursor when using the current selection" $
            forAllValid $ \tfc -> do
                let (x, y) = textFieldCursorSelection tfc
                    t = rebuildTextFieldCursor tfc
                case makeTextFieldCursorWithSelection x y t of
                    Nothing ->
                        expectationFailure
                            "makeTextFieldCursorWithSelection should not have failed."
                    Just tfc' ->
                        unless (tfc' == tfc) $
                        expectationFailure $
                        unlines
                            [ "expected"
                            , ppShow tfc
                            , "actual"
                            , ppShow tfc'
                            , "The selection of the original (expected) cursor was:"
                            , show (x, y)
                            , "The rebuild text was:"
                            , show t
                            ]
    describe "rebuildTextFieldCursorLines" $ do
        it "produces valid lists" $
            producesValidsOnValids rebuildTextFieldCursorLines
        it "produces texts without newlines" $
            forAllValid $ \tfc -> do
                let ls = NE.toList $ rebuildTextFieldCursorLines tfc
                unless (all (T.all (/= '\n')) ls) $
                    expectationFailure $
                    unlines $
                    "Some of the following lines contain a newline:" :
                    map show ls
    describe "rebuildTextFieldCursor" $ do
        it "produces valid texts" $
            producesValidsOnValids rebuildTextFieldCursor
        it "is the inverse of makeTextFieldCursor for integers" $
            inverseFunctionsOnValid makeTextFieldCursor rebuildTextFieldCursor
        it
            "is the inverse of makeTextFieldCursorWithSelection for integers, for any index" $
            forAllValid $ \x ->
                forAllValid $ \y ->
                    inverseFunctionsIfFirstSucceedsOnValid
                        (makeTextFieldCursorWithSelection x y)
                        rebuildTextFieldCursor
    describe "textFieldCursorSelection" $
        it "produces valid tuples" $
        producesValidsOnValids textFieldCursorSelection
    describe "emptyTextFieldCursor" $
        it "is valid" $ shouldBeValid emptyTextFieldCursor
    describe "nullTextFieldCursor" $
        it "produces valid" $ producesValidsOnValids nullTextFieldCursor
    describe "textFieldCursorSelectPrevLine" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectPrevLine
        it "is a movement" $ isMovementM textFieldCursorSelectPrevLine
        it "selects the previous line" pending
    describe "textFieldCursorSelectNextLine" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectNextLine
        it "is a movement" $ isMovementM textFieldCursorSelectNextLine
        it "selects the next line" pending
    describe "textFieldCursorSelectFirstLine" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectFirstLine
        it "is a movement" $ isMovement textFieldCursorSelectFirstLine
        it "is idempotent" $ idempotentOnValid textFieldCursorSelectFirstLine
        it "selects the first line" pending
    describe "textFieldCursorSelectLastLine" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectLastLine
        it "is a movement" $ isMovement textFieldCursorSelectLastLine
        it "is idempotent" $ idempotentOnValid textFieldCursorSelectLastLine
        it "selects the last line" pending
    describe "textFieldCursorSelectPrevChar" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectPrevChar
        it "selects the previous character on the current line" pending
    describe "textFieldCursorSelectNextChar" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectNextChar
        it "selects the previous character on the current line" pending
    describe "textFieldCursorIndexOnLine" $ do
        it "produces valid indices" $
            producesValidsOnValids textFieldCursorIndexOnLine
        it "returns the index on the current line" pending
    describe "textFieldCursorSelectIndexOnLine" $ do
        it "produces valid cursors for any index" $
            producesValidsOnValids2 textFieldCursorSelectIndexOnLine
        it "selects the given index on the current line" pending
    describe "textFieldCursorInsertChar" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (textFieldCursorInsertChar d)
        it "inserts a character before the cursor on the current line" pending
    describe "textFieldCursorAppendChar" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (textFieldCursorAppendChar d)
        it "inserts a character after the cursor on the currrent line" pending
    describe "textFieldCursorInsertNewline" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorInsertNewline
        it "inserts a new line" pending
    describe "textFieldCursorAppendNewline" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorAppendNewline
    describe "textFieldCursorRemove" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorRemove
        it "removes empty text field cursor" $
            textFieldCursorRemove emptyTextFieldCursor `shouldBe` Just Deleted
        it "removes a character or a line" pending
    describe "textFieldCursorDelete" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorDelete
        it "removes empty text field cursor" $
            textFieldCursorDelete emptyTextFieldCursor `shouldBe` Just Deleted
        it "deletes a character or a line" pending
    describe "textFieldCursorSelectStartOfLine" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectStartOfLine
        it "selects the start of the current line" pending
    describe "textFieldCursorSelectEndOfLine" $ do
        it "produces valid cursors" $
            producesValidsOnValids textFieldCursorSelectEndOfLine
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
                        [ "Cursor before:\n" ++ show tfc
                        , "TextField before:  \n" ++ show tf
                        , "Cursor after: \n" ++ show tfc'
                        , "TextField after:   \n" ++ show tf'
                        ]

isMovement :: (TextFieldCursor -> TextFieldCursor) -> Property
isMovement func =
    forAllValid $ \tfc ->
        rebuildTextFieldCursor tfc `shouldBe` rebuildTextFieldCursor (func tfc)
