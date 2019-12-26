{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.TextField.Gen where

import Data.GenValidity
import Data.GenValidity.Text

import Test.QuickCheck

import Cursor.List.NonEmpty
import Cursor.TextField

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen

instance GenValid TextFieldCursor where
  genValid =
    sized $ \n -> do
      (a, b, c) <- genSplit3 n
      prevs <- resize a $ genListOf $ genTextBy genTextCursorChar
      nexts <- resize b $ genListOf $ genTextBy genTextCursorChar
      cur <- resize c $ textCursorWithGen genTextCursorChar
      let nec = NonEmptyCursor prevs cur nexts
      pure $ TextFieldCursor nec
  shrinkValid = shrinkValidStructurally
