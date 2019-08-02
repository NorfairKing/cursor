{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Simple.Tree.CollapseSpec
  ( spec
  ) where

import Test.Hspec

import Test.Validity

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()

spec :: Spec
spec = do
  describe "treeCursorOpenCurrentForest" $
    it "produces valid cursors" $
    producesValidsOnValids $ treeCursorOpenCurrentForest @Rational @Rational
  describe "treeCursorCloseCurrentForest" $
    it "produces valid cursors" $
    producesValidsOnValids $ treeCursorCloseCurrentForest @Rational @Rational
  describe "treeCursorToggleCurrentForest" $
    it "produces valid cursors" $
    producesValidsOnValids $ treeCursorToggleCurrentForest @Rational @Rational
  describe "treeCursorOpenCurrentForestRecursively" $
    it "produces valid cursors" $
    producesValidsOnValids $
    treeCursorOpenCurrentForestRecursively @Double @Double
  describe "treeCursorToggleCurrentForestRecursively" $
    it "produces valid cursors" $
    producesValidsOnValids $
    treeCursorToggleCurrentForestRecursively @Double @Double
