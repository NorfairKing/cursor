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
  eqSpec @(SwapResult Bool)
  genValidSpec @(SwapResult Bool)
  shrinkValidSpecWithLimit @(SwapResult Bool) 10
  eqSpec @(PromoteElemResult Bool)
  genValidSpec @(PromoteElemResult Bool)
  shrinkValidSpecWithLimit @(PromoteElemResult Bool) 10
  eqSpec @(PromoteResult Bool)
  genValidSpec @(PromoteResult Bool)
  shrinkValidSpecWithLimit @(PromoteResult Bool) 10
  eqSpec @(DemoteResult Bool)
  genValidSpec @(DemoteResult Bool)
  shrinkValidSpecWithLimit @(DemoteResult Bool) 10
  eqSpec @(CTree Bool)
  genValidSpec @(CTree Bool)
  shrinkValidSpecWithLimit @(CTree Bool) 10
  eqSpec @(CForest Bool)
  genValidSpec @(CForest Bool)
  shrinkValidSpecWithLimit @(CForest Bool) 10
  eqSpec @(TreeAbove Bool)
  genValidSpec @(TreeAbove Bool)
  shrinkValidSpecWithLimit @(TreeAbove Bool) 10
  describe "treeAboveLeftsL" $ lensSpecOnValid $ treeAboveLeftsL @Bool
  describe "treeAboveAboveL" $ lensSpecOnValid $ treeAboveAboveL @Bool
  describe "treeAboveNodeL" $ lensSpecOnValid $ treeAboveNodeL @Bool
  describe "treeAboveRightsL" $ lensSpecOnValid $ treeAboveRightsL @Bool
  eqSpec @(TreeCursor Bool Word)
  genValidSpec @(TreeCursor Bool Bool)
  shrinkValidSpecWithLimit @(TreeCursor Word Bool) 10
  describe "treeCursorAboveL" $ lensSpecOnValid $ treeCursorAboveL @Bool @Bool
  describe "treeCursorCurrentL" $ lensSpecOnValid $ treeCursorCurrentL @Bool @Bool
  describe "treeCursorBelowL" $ lensSpecOnValid $ treeCursorBelowL @Bool @Bool
  describe "treeCursorCurrentSubTreeL" $ lensSpecOnValid $ treeCursorCurrentSubTreeL @Bool @Bool
