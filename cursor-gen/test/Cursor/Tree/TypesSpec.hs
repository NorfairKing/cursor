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
  shrinkValidSpecWithLimit @TreeCursorSelection 10
  eqSpec @(SwapResult Int)
  genValidSpec @(SwapResult Rational)
  shrinkValidSpecWithLimit @(SwapResult Int) 10
  eqSpec @(PromoteElemResult Int)
  genValidSpec @(PromoteElemResult Rational)
  shrinkValidSpecWithLimit @(PromoteElemResult Int) 10
  eqSpec @(PromoteResult Int)
  genValidSpec @(PromoteResult Rational)
  shrinkValidSpecWithLimit @(PromoteResult Int) 10
  eqSpec @(DemoteResult Int)
  genValidSpec @(DemoteResult Rational)
  shrinkValidSpecWithLimit @(DemoteResult Int) 10
  eqSpec @(CTree Int)
  genValidSpec @(CTree Rational)
  shrinkValidSpecWithLimit @(CTree Int) 10
  eqSpec @(CForest Int)
  genValidSpec @(CForest Rational)
  shrinkValidSpecWithLimit @(CForest Int) 10
  eqSpec @(TreeAbove Int)
  genValidSpec @(TreeAbove Rational)
  shrinkValidSpecWithLimit @(TreeAbove Int) 10
  describe "treeAboveLeftsL" $ lensSpecOnValid $ treeAboveLeftsL @Rational
  describe "treeAboveAboveL" $ lensSpecOnValid $ treeAboveAboveL @Rational
  describe "treeAboveNodeL" $ lensSpecOnValid $ treeAboveNodeL @Rational
  describe "treeAboveRightsL" $ lensSpecOnValid $ treeAboveRightsL @Rational
  eqSpec @(TreeCursor Int Word)
  genValidSpec @(TreeCursor Rational Rational)
  shrinkValidSpecWithLimit @(TreeCursor Word Int) 10
  describe "treeCursorAboveL" $
    lensSpecOnValid $ treeCursorAboveL @Rational @Rational
  describe "treeCursorCurrentL" $
    lensSpecOnValid $ treeCursorCurrentL @Rational @Rational
  describe "treeCursorBelowL" $
    lensSpecOnValid $ treeCursorBelowL @Rational @Rational
  describe "treeCursorCurrentSubTreeL" $
    lensSpecOnValid $ treeCursorCurrentSubTreeL @Rational @Rational
