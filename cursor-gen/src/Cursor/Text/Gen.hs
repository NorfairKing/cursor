{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Text.Gen
  ( genSafeChar,
    genTextCursorChar,
    textCursorWithGen,
    textCursorWithIndex0,
  )
where

import Cursor.List.Gen
import Cursor.Text
import Cursor.Types
import Data.GenValidity
import Data.GenValidity.Text ()
import Test.QuickCheck

instance GenUnchecked TextCursor

instance GenValid TextCursor where
  genValid = TextCursor <$> listCursorWithGen genTextCursorChar
  shrinkValid = shrinkValidStructurally

genSafeChar :: Gen Char
genSafeChar = choose (minBound, maxBound) `suchThat` isSafeChar

genTextCursorChar :: Gen Char
genTextCursorChar = genSafeChar `suchThat` (/= '\n')

textCursorWithGen :: Gen Char -> Gen TextCursor
textCursorWithGen gen = TextCursor <$> listCursorWithGen gen

textCursorWithIndex0 :: Gen Char -> Gen TextCursor
textCursorWithIndex0 gen = TextCursor <$> listCursorWithIndex0 gen
