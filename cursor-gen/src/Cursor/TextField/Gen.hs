{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.TextField.Gen where

import qualified Data.Text as T
import qualified Data.Text.Internal as T

import Data.GenValidity

import Test.QuickCheck

import Cursor.List.NonEmpty
import Cursor.TextField

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen

instance GenValid TextFieldCursor where
  genValid = do
    let charGen = genValid `suchThat` (/= '\n') `suchThat` (\c -> T.safe c == c)
    prevs <- genListOf $ T.pack <$> genListOf charGen
    nexts <- genListOf $ T.pack <$> genListOf charGen
    cur <- textCursorWithGen charGen
    let nec = NonEmptyCursor prevs cur nexts
    pure $ TextFieldCursor nec
  shrinkValid = shrinkValidStructurally
