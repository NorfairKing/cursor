{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.TextField.Gen where

import qualified Data.Sequence as S
import qualified Data.Text as T

import Data.GenValidity

import Test.QuickCheck

import Cursor.List.NonEmpty
import Cursor.TextField

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen

instance GenValid TextFieldCursor where
  genValid = do
    let charGen = genValid `suchThat` (/= '\n')
    let genLine = T.pack <$> genListOf charGen
    let genLineSeq = S.fromList <$> genListOf genLine
    prevs <- genLineSeq
    nexts <- genLineSeq
    cur <- textCursorWithGen charGen
    let nec = NonEmptyCursor prevs cur nexts
    pure $ TextFieldCursor nec
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
