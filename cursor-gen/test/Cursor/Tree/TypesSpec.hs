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
    shrinkValidSpecWithLimit @TreeCursorSelection 100
    eqSpec @(SwapResult Int)
    genValidSpec @(SwapResult Double)
    shrinkValidSpecWithLimit @(SwapResult Int) 100
    eqSpec @(PromoteElemResult Int)
    genValidSpec @(PromoteElemResult Double)
    shrinkValidSpecWithLimit @(PromoteElemResult Int) 100
    eqSpec @(PromoteResult Int)
    genValidSpec @(PromoteResult Double)
    shrinkValidSpecWithLimit @(PromoteResult Int) 100
    eqSpec @(DemoteResult Int)
    genValidSpec @(DemoteResult Double)
    shrinkValidSpecWithLimit @(DemoteResult Int) 100
    eqSpec @(CTree Int)
    genValidSpec @(CTree Double)
    shrinkValidSpecWithLimit @(CTree Int) 100
    eqSpec @(CForest Int)
    genValidSpec @(CForest Double)
    shrinkValidSpecWithLimit @(CForest Int) 100
    eqSpec @(TreeAbove Int)
    genValidSpec @(TreeAbove Double)
    shrinkValidSpecWithLimit @(TreeAbove Int) 100
    describe "treeAboveLeftsL" $ lensSpecOnValid $ treeAboveLeftsL @Double
    describe "treeAboveAboveL" $ lensSpecOnValid $ treeAboveAboveL @Double
    describe "treeAboveNodeL" $ lensSpecOnValid $ treeAboveNodeL @Double
    describe "treeAboveRightsL" $ lensSpecOnValid $ treeAboveRightsL @Double
    eqSpec @(TreeCursor Int Word)
    genValidSpec @(TreeCursor Float Double)
    shrinkValidSpecWithLimit @(TreeCursor Word Int) 100
    describe "treeCursorAboveL" $
        lensSpecOnValid $ treeCursorAboveL @Float @Double
    describe "treeCursorCurrentL" $
        lensSpecOnValid $ treeCursorCurrentL @Float @Double
    describe "treeCursorBelowL" $
        lensSpecOnValid $ treeCursorBelowL @Float @Double
    describe "treeCursorCurrentSubTreeL" $
        lensSpecOnValid $ treeCursorCurrentSubTreeL @Float @Double
