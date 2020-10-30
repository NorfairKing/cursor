{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Text.Gen
  ( genSafeChar
  , genTextCursorChar
  , textCursorSentenceGen
  , textCursorWithGen
  , textCursorWithIndex0
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

genSpaceChar :: Gen Char
genSpaceChar = elements [' ', '\t', '\v']

genTextCursorChar :: Gen Char
genTextCursorChar = genSafeChar `suchThat` (/= '\n')

textCursorWithGen :: Gen Char -> Gen TextCursor
textCursorWithGen gen = TextCursor <$> listCursorWithGen gen

textCursorWithIndex0 :: Gen Char -> Gen TextCursor
textCursorWithIndex0 gen = TextCursor <$> listCursorWithIndex0 gen

textCursorSentenceGen :: Gen TextCursor
textCursorSentenceGen = TextCursor <$> listCursorWithGen sentenceGen
  where
    sentenceGen :: Gen Char
    sentenceGen = frequency [(1, genSpaceChar), (9, genSafeChar)]
