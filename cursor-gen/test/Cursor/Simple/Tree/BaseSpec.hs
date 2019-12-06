{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Simple.Tree.BaseSpec
  ( spec
  ) where

import Test.Hspec

import Test.Validity

import Cursor.Simple.Tree hiding (TreeCursor)
import qualified Cursor.Simple.Tree as STC (TreeCursor)
import Cursor.Simple.Tree.Gen ()

import Cursor.Simple.Tree.TestUtils

spec :: Spec
spec = do
  eqSpec @(STC.TreeCursor Int)
  genValidSpec @(STC.TreeCursor Bool)
  describe "makeTreeCursor" $
    it "produces valid cursors" $ producesValidsOnValids (makeTreeCursor @Bool)
  describe "makeTreeCursorWithSelection" $
    it "produces valid cursors" $ producesValidsOnValids2 (makeTreeCursorWithSelection @Bool)
  describe "singletonTreeCursor" $
    it "produces valid cursors" $ producesValidsOnValids (singletonTreeCursor @Bool)
  describe "rebuildTreeCursor" $ do
    it "produces valid trees" $ producesValidsOnValids (rebuildTreeCursor @Bool)
    it "is the inverse of makeTreeCursor for integers" $
      inverseFunctions (makeTreeCursor @Int) rebuildTreeCursor
    it "is the inverse of makeTreeCursorWithSelection for the current selection" $
      forAllValid $ \tc ->
        case makeTreeCursorWithSelection @Bool (treeCursorSelection tc) (rebuildTreeCursor tc) of
          Nothing -> expectationFailure "makeTreeCursorWithSelection should not have failed."
          Just r -> r `treeShouldBe` tc
