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
    it "produces valid cursors" $ producesValidsOnValids $ treeCursorOpenCurrentForest @Bool @Bool
  describe "treeCursorCloseCurrentForest" $
    it "produces valid cursors" $ producesValidsOnValids $ treeCursorCloseCurrentForest @Bool @Bool
  describe "treeCursorToggleCurrentForest" $
    it "produces valid cursors" $ producesValidsOnValids $ treeCursorToggleCurrentForest @Bool @Bool
  describe "treeCursorOpenCurrentForestRecursively" $
    it "produces valid cursors" $
    producesValidsOnValids $ treeCursorOpenCurrentForestRecursively @Bool @Bool
  describe "treeCursorToggleCurrentForestRecursively" $
    it "produces valid cursors" $
    producesValidsOnValids $ treeCursorToggleCurrentForestRecursively @Bool @Bool
