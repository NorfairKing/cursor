{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.TextField.Gen where

import Test.QuickCheck

import Data.GenValidity

import Cursor.NonEmpty
import Cursor.TextField

import Cursor.NonEmpty.Gen ()
import Cursor.Text.Gen

instance GenUnchecked TextFieldCursor

instance GenValid TextFieldCursor where
    genValid =
        (do let charGen = genValid `suchThat` (/= '\n')
            prevs <- genListOf $ textCursorWithIndex0 charGen
            nexts <- genListOf $ textCursorWithIndex0 charGen
            cur <- textCursorWithGen charGen
            let nec = NonEmptyCursor prevs cur nexts
            pure $ TextFieldCursor nec) `suchThat`
        isValid
