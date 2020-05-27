{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.List.NonEmpty.Gen
  ( genNonEmptyCursorBy,
    nonEmptyElemOf,
    nonEmptyWithIndex0,
    nonEmptyWith,
  )
where

import Control.Monad
import Cursor.List.NonEmpty
import Data.GenValidity
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (NonEmptyCursor a b) where
  genUnchecked = genNonEmptyCursorBy genUnchecked genUnchecked
  shrinkUnchecked (NonEmptyCursor prev cur next) =
    [NonEmptyCursor prev' cur' next' | (prev', cur', next') <- shrinkUnchecked (prev, cur, next)]

instance (GenValid a, GenValid b) => GenValid (NonEmptyCursor a b) where
  genValid = genNonEmptyCursorBy genValid genValid
  shrinkValid (NonEmptyCursor prev cur next) =
    [NonEmptyCursor prev' cur' next' | (prev', cur', next') <- shrinkValid (prev, cur, next)]

genNonEmptyCursorBy :: Gen a -> Gen b -> Gen (NonEmptyCursor a b)
genNonEmptyCursorBy genA genB =
  sized $ \n -> do
    part <- arbPartition n
    case part of
      [] -> singletonNonEmptyCursor <$> resize 0 genA
      (s : ss) -> do
        i <- choose (0, length ss)
        let (as, bs) = splitAt i ss
        nonEmptyCursorPrev <- forM as $ \s_ -> resize s_ genB
        nonEmptyCursorCurrent <- resize s genA
        nonEmptyCursorNext <- forM bs $ \s_ -> resize s_ genB
        pure NonEmptyCursor {..}

nonEmptyElemOf :: NonEmptyCursor a a -> Gen a
nonEmptyElemOf = elements . NE.toList . rebuildNonEmptyCursor id

nonEmptyWithIndex0 :: Gen a -> Gen (NonEmptyCursor a a)
nonEmptyWithIndex0 g = NonEmptyCursor [] <$> g <*> genListOf g

nonEmptyWith :: a -> Gen a -> Gen (NonEmptyCursor a a)
nonEmptyWith a g =
  oneof
    [ NonEmptyCursor <$> listWithA <*> g <*> genListOf g,
      NonEmptyCursor <$> genListOf g <*> pure a <*> genListOf g,
      NonEmptyCursor <$> genListOf g <*> g <*> listWithA
    ]
  where
    listWithA = do
      l1 <- genListOf g
      l2 <- genListOf g
      pure $ l1 ++ [a] ++ l2
