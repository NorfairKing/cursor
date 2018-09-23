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
    eqSpec @(SwapResult Int)
    genValidSpec @(SwapResult Double)
    eqSpec @(PromoteElemResult Int)
    genValidSpec @(PromoteElemResult Double)
    eqSpec @(PromoteResult Int)
    genValidSpec @(PromoteResult Double)
    eqSpec @(DemoteResult Int)
    genValidSpec @(DemoteResult Double)
    eqSpec @(Collapse Int)
    genValidSpec @(Collapse Double)
    describe "collapseValueL" $ lensSpecOnValid $ collapseValueL @Double
    describe "collapseShowL" $ lensSpecOnValid $ collapseShowL @Double
    eqSpec @(CTree Int)
    genValidSpec @(CTree Double)
    eqSpec @(TreeAbove Int)
    genValidSpec @(TreeAbove Double)
    describe "treeAboveLeftsL" $ lensSpecOnValid $ treeAboveLeftsL @Double
    describe "treeAboveAboveL" $ lensSpecOnValid $ treeAboveAboveL @Double
    describe "treeAboveNodeL" $ lensSpecOnValid $ treeAboveNodeL @Double
    describe "treeAboveRightsL" $ lensSpecOnValid $ treeAboveRightsL @Double
    eqSpec @(TreeCursor Int Word)
    genValidSpec @(TreeCursor Float Double)
    describe "treeCursorAboveL" $
        lensSpecOnValid $ treeCursorAboveL @Float @Double
    describe "treeCursorCurrentL" $
        lensSpecOnValid $ treeCursorCurrentL @Float @Double
    describe "treeCursorBelowL" $
        lensSpecOnValid $ treeCursorBelowL @Float @Double
