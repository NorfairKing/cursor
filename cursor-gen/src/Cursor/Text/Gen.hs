{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Text.Gen
  ( genSafeChar,
    genTextCursorChar,
    textCursorSentenceGen,
    textCursorWithGen,
    textCursorWithIndex0,
    shrinkSentence,
  )
where

import Cursor.List
import Cursor.List.Gen
import Cursor.Text
import Cursor.Types
import Data.Char (isSpace)
import Data.GenValidity
import Data.GenValidity.Text ()
import Test.QuickCheck

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
textCursorSentenceGen = textCursorWithGen sentenceGen
  where
    sentenceGen :: Gen Char
    sentenceGen = frequency [(1, genSpaceChar), (5, genSafeChar)]

shrinkSentence :: TextCursor -> [TextCursor]
shrinkSentence tc@(TextCursor (ListCursor before after)) =
  filter (/= tc) [TextCursor (ListCursor (map f before) (map f after))]
  where
    f :: Char -> Char
    f x
      | isSpace x = ' '
      | otherwise = 'a'
