{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Tree.TypesSpec
  ( spec,
  )
where

import Cursor.Tree
import Cursor.Tree.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Optics

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
  describe "treeAboveLeftsL" $ lensSpec $ treeAboveLeftsL @Bool
  describe "treeAboveAboveL" $ lensSpec $ treeAboveAboveL @Bool
  describe "treeAboveNodeL" $ lensSpec $ treeAboveNodeL @Bool
  describe "treeAboveRightsL" $ lensSpec $ treeAboveRightsL @Bool
  eqSpec @(TreeCursor Bool Word)
  genValidSpec @(TreeCursor Bool Bool)
  shrinkValidSpecWithLimit @(TreeCursor Word Bool) 10
  describe "treeCursorAboveL" $ lensSpec $ treeCursorAboveL @Bool @Bool
  describe "treeCursorCurrentL" $ lensSpec $ treeCursorCurrentL @Bool @Bool
  describe "treeCursorBelowL" $ lensSpec $ treeCursorBelowL @Bool @Bool
  describe "treeCursorCurrentSubTreeL" $ lensSpec $ treeCursorCurrentSubTreeL @Bool @Bool
