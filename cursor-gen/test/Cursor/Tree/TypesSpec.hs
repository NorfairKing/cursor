{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Tree.TypesSpec
    ( spec
    ) where

import Test.Hspec

import Test.Validity
import Test.Validity.Optics

import Cursor.Tree
import Cursor.Tree.Gen ()

spec :: Spec
spec = do
        eqSpec @TreeCursorSelection
        genValidSpec @TreeCursorSelection
        shrinkValidSpec @TreeCursorSelection
        eqSpec @(SwapResult Int)
        genValidSpec @(SwapResult Double)
        shrinkValidSpec @(SwapResult Int)
        eqSpec @(PromoteElemResult Int)
        genValidSpec @(PromoteElemResult Double)
        shrinkValidSpec @(PromoteElemResult Int)
        eqSpec @(PromoteResult Int)
        genValidSpec @(PromoteResult Double)
        shrinkValidSpec @(PromoteResult Int)
        eqSpec @(DemoteResult Int)
        genValidSpec @(DemoteResult Double)
        shrinkValidSpec @(DemoteResult Int)
        eqSpec @(CTree Int)
        genValidSpec @(CTree Double)
        shrinkValidSpec @(CTree Int)
        eqSpec @(CForest Int)
        genValidSpec @(CForest Double)
        shrinkValidSpec @(CForest Int)
        eqSpec @(TreeAbove Int)
        genValidSpec @(TreeAbove Double)
        shrinkValidSpec @(TreeAbove Int)
        describe "treeAboveLeftsL" $ lensSpecOnValid $ treeAboveLeftsL @Double
        describe "treeAboveAboveL" $ lensSpecOnValid $ treeAboveAboveL @Double
        describe "treeAboveNodeL" $ lensSpecOnValid $ treeAboveNodeL @Double
        describe "treeAboveRightsL" $ lensSpecOnValid $ treeAboveRightsL @Double
        eqSpec @(TreeCursor Int Word)
        genValidSpec @(TreeCursor Float Double)
        shrinkValidSpec @(TreeCursor Word Int)
        describe "treeCursorAboveL" $
            lensSpecOnValid $ treeCursorAboveL @Float @Double
        describe "treeCursorCurrentL" $
            lensSpecOnValid $ treeCursorCurrentL @Float @Double
        describe "treeCursorBelowL" $
            lensSpecOnValid $ treeCursorBelowL @Float @Double
        describe "treeCursorCurrentSubTreeL" $
            lensSpecOnValid $ treeCursorCurrentSubTreeL @Float @Double
