{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Text.Gen
    ( textCursorWithGen
    , textCursorWithIndex0
    ) where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Text ()

import Cursor.Text

import Cursor.List.Gen

instance GenUnchecked TextCursor

instance GenValid TextCursor where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

textCursorWithGen :: Gen Char -> Gen TextCursor
textCursorWithGen gen = TextCursor <$> listCursorWithGen gen

textCursorWithIndex0 :: Gen Char -> Gen TextCursor
textCursorWithIndex0 gen = TextCursor <$> listCursorWithIndex0 gen
