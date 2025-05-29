{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Tree.Gen
  (
  )
where

import Cursor.Tree
import Data.GenValidity
import Data.GenValidity.Containers ()
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck

instance GenValid TreeCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (SwapResult a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (PromoteElemResult a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (PromoteResult a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (DemoteResult a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (CTree a) where
  genValid =
    sized $ \n -> do
      (a, b) <- genSplit n
      val <- resize a genValid
      for <- resize b genValid
      pure $ CNode val for
  shrinkValid (CNode a cf) = [CNode a' cf' | (a', cf') <- shrinkValid (a, cf)]

instance (GenValid a) => GenValid (CForest a) where
  genValid =
    sized $ \case
      0 -> pure EmptyCForest
      _ -> oneof [ClosedForest <$> genValid, OpenForest <$> genValid]
  shrinkValid EmptyCForest = []
  shrinkValid (ClosedForest ne) = EmptyCForest : (ClosedForest <$> shrinkValid ne)
  shrinkValid (OpenForest ne) =
    EmptyCForest : ClosedForest (NE.map rebuildCTree ne) : (OpenForest <$> shrinkValid ne)

instance (GenValid a, GenValid b) => GenValid (TreeCursor a b) where
  genValid =
    sized $ \n -> do
      s <- upTo n
      (a, b, c, d) <- genSplit4 s
      treeAbove <- resize (a + b) genValid
      treeCurrent <- resize c genValid
      treeBelow <- resize d genValid
      pure TreeCursor {..}
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid b) => GenValid (TreeAbove b) where
  genValid =
    sized $ \n -> do
      s <- upTo n
      (a, b, c, d) <- genSplit4 s
      treeAboveLefts <- resize a genValid
      treeAboveAbove <- resize b genValid
      treeAboveNode <- resize c genValid
      treeAboveRights <- resize d genValid
      pure TreeAbove {..}
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
