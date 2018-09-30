{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.TextField.Gen where

import qualified Data.Text as T

import Data.GenValidity

import Test.QuickCheck

import Cursor.List.NonEmpty
import Cursor.TextField

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen

instance GenUnchecked TextFieldCursor

instance GenValid TextFieldCursor where
    genValid = do
        let charGen = genValid `suchThat` (/= '\n')
        prevs <- genListOf $ T.pack <$> genListOf charGen
        nexts <- genListOf $ T.pack <$> genListOf charGen
        cur <- textCursorWithGen charGen
        let nec = NonEmptyCursor prevs cur nexts
        pure $ TextFieldCursor nec
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
