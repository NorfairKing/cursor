{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.List.NonEmpty.Gen
  ( nonEmptyElemOf
  , nonEmptyWithIndex0
  , nonEmptyWith
  ) where

import Control.Monad

import Data.Sequence (Seq(..))
import qualified Data.Sequence as S

import Data.GenValidity
import Data.GenValidity.Sequence ()

import Test.QuickCheck

import qualified Data.List.NonEmpty as NE

import Cursor.List.NonEmpty

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (NonEmptyCursor a b) where
  genUnchecked =
    sized $ \n -> do
      part <- S.fromList <$> arbPartition n
      case part of
        Empty -> singletonNonEmptyCursor <$> resize 0 genUnchecked
        s :<| ss -> do
          i <- choose (0, length ss)
          let (as, bs) = S.splitAt i ss
          nonEmptyCursorPrev <- forM as $ \s_ -> resize s_ genUnchecked
          nonEmptyCursorCurrent <- resize s genUnchecked
          nonEmptyCursorNext <- forM bs $ \s_ -> resize s_ genUnchecked
          pure NonEmptyCursor {..}
  shrinkUnchecked (NonEmptyCursor prev cur next) =
    [NonEmptyCursor prev' cur' next' | (prev', cur', next') <- shrinkUnchecked (prev, cur, next)]

instance (GenValid a, GenValid b) => GenValid (NonEmptyCursor a b) where
  genValid =
    sized $ \n -> do
      part <- S.fromList <$> arbPartition n
      case part of
        Empty -> singletonNonEmptyCursor <$> resize 0 genValid
        s :<| ss -> do
          i <- choose (0, length ss)
          let (as, bs) = S.splitAt i ss
          nonEmptyCursorPrev <- forM as $ \s_ -> resize s_ genValid
          nonEmptyCursorCurrent <- resize s genValid
          nonEmptyCursorNext <- forM bs $ \s_ -> resize s_ genValid
          pure NonEmptyCursor {..}
  shrinkValid (NonEmptyCursor prev cur next) =
    [NonEmptyCursor prev' cur' next' | (prev', cur', next') <- shrinkValid (prev, cur, next)]

nonEmptyElemOf :: NonEmptyCursor a a -> Gen a
nonEmptyElemOf = elements . NE.toList . rebuildNonEmptyCursor id

nonEmptyWithIndex0 :: Gen a -> Gen (NonEmptyCursor a a)
nonEmptyWithIndex0 g = NonEmptyCursor S.empty <$> g <*> genSeqOf g

nonEmptyWith :: forall a. a -> Gen a -> Gen (NonEmptyCursor a a)
nonEmptyWith a g =
  oneof
    [ NonEmptyCursor <$> seqWithA <*> g <*> genSeqOf g
    , NonEmptyCursor <$> genSeqOf g <*> pure a <*> genSeqOf g
    , NonEmptyCursor <$> genSeqOf g <*> g <*> seqWithA
    ]
  where
    seqWithA :: Gen (Seq a)
    seqWithA = do
      l1 <- genListOf g
      l2 <- genListOf g
      pure $ S.fromList $ l1 ++ [a] ++ l2

genSeqOf :: Gen a -> Gen (Seq a)
genSeqOf = fmap S.fromList . genListOf
