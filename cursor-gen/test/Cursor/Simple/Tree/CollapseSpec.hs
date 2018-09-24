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
        producesValidsOnValids $ treeCursorOpenCurrentForest @Double @Double
    describe "treeCursorCloseCurrentForest" $
        it "produces valid cursors" $
        producesValidsOnValids $ treeCursorCloseCurrentForest @Double @Double
    describe "treeCursorToggleCurrentForest" $
        it "produces valid cursors" $
        producesValidsOnValids $ treeCursorToggleCurrentForest @Double @Double
