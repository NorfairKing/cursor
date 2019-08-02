{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.List.NonEmpty
  ( NonEmptyCursor(..)
  , makeNonEmptyCursor
  , makeNonEmptyCursorWithSelection
  , singletonNonEmptyCursor
  , rebuildNonEmptyCursor
  , nonEmptyCursorElemL
  , mapNonEmptyCursor
  , nonEmptyCursorSelectPrev
  , nonEmptyCursorSelectNext
  , nonEmptyCursorSelectFirst
  , nonEmptyCursorSelectLast
  , nonEmptyCursorSelection
  , nonEmptyCursorSelectIndex
  , nonEmptyCursorInsert
  , nonEmptyCursorAppend
  , nonEmptyCursorInsertAndSelect
  , nonEmptyCursorAppendAndSelect
  , nonEmptyCursorInsertAtStart
  , nonEmptyCursorAppendAtEnd
  , nonEmptyCursorInsertAtStartAndSelect
  , nonEmptyCursorAppendAtEndAndSelect
  , nonEmptyCursorRemoveElemAndSelectPrev
  , nonEmptyCursorDeleteElemAndSelectNext
  , nonEmptyCursorRemoveElem
  , nonEmptyCursorDeleteElem
  , nonEmptyCursorSearch
  , nonEmptyCursorSelectOrAdd
  , nonemptyPrepend
  , nonemptyAppend
  , traverseNonEmptyCursor
  , foldNonEmptyCursor
  ) where

import GHC.Generics (Generic)

import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as S
import Data.Sequence (Seq(..), ViewL(..), ViewR(..), (<|), (|>))
import Data.Validity
import Data.Validity.Sequence ()

import Control.Monad

import Lens.Micro

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Cursor.Types

-- | A 'nonempty list' cursor
data NonEmptyCursor a b =
  NonEmptyCursor
    { nonEmptyCursorPrev :: Seq b
    , nonEmptyCursorCurrent :: a
    , nonEmptyCursorNext :: Seq b
    }
  deriving (Show, Eq, Generic, Functor)

instance (Validity a, Validity b) => Validity (NonEmptyCursor a b)

makeNonEmptyCursor :: (b -> a) -> NonEmpty b -> NonEmptyCursor a b
makeNonEmptyCursor g = fromJust . makeNonEmptyCursorWithSelection g 0

makeNonEmptyCursorWithSelection :: (b -> a) -> Int -> NonEmpty b -> Maybe (NonEmptyCursor a b)
makeNonEmptyCursorWithSelection g i ne = do
  (l, m, r) <- applyNonEmptySelection ne i
  pure
    NonEmptyCursor
      { nonEmptyCursorPrev = S.fromList l
      , nonEmptyCursorCurrent = g m
      , nonEmptyCursorNext = S.fromList r
      }
  where
    applyNonEmptySelection :: NonEmpty a -> Int -> Maybe ([a], a, [a])
    applyNonEmptySelection (c :| rest) i_
      | i_ < 0 = Nothing
      | i_ == 0 = Just ([], c, rest)
      | otherwise = do
        ne_ <- NE.nonEmpty rest
        (l, m, r) <- applyNonEmptySelection ne_ (i_ - 1)
        pure (c : l, m, r)

singletonNonEmptyCursor :: a -> NonEmptyCursor a b
singletonNonEmptyCursor a =
  NonEmptyCursor
    {nonEmptyCursorPrev = S.empty, nonEmptyCursorCurrent = a, nonEmptyCursorNext = S.empty}

rebuildNonEmptyCursor :: (a -> b) -> NonEmptyCursor a b -> NonEmpty b
rebuildNonEmptyCursor f NonEmptyCursor {..} =
  nonemptyPrepend nonEmptyCursorPrev $ f nonEmptyCursorCurrent :| (toList nonEmptyCursorNext)

mapNonEmptyCursor :: (a -> c) -> (b -> d) -> NonEmptyCursor a b -> NonEmptyCursor c d
mapNonEmptyCursor f g NonEmptyCursor {..} =
  NonEmptyCursor
    { nonEmptyCursorPrev = fmap g nonEmptyCursorPrev
    , nonEmptyCursorCurrent = f nonEmptyCursorCurrent
    , nonEmptyCursorNext = fmap g nonEmptyCursorNext
    }

nonEmptyCursorElemL :: Lens (NonEmptyCursor a c) (NonEmptyCursor b c) a b
nonEmptyCursorElemL = lens nonEmptyCursorCurrent $ \lec le -> lec {nonEmptyCursorCurrent = le}

nonEmptyCursorSelectPrev :: (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectPrev f g lec =
  case S.viewr $ nonEmptyCursorPrev lec of
    EmptyR -> Nothing
    rest :> e ->
      Just $
      lec
        { nonEmptyCursorPrev = rest
        , nonEmptyCursorCurrent = g e
        , nonEmptyCursorNext = f (nonEmptyCursorCurrent lec) <| nonEmptyCursorNext lec
        }

nonEmptyCursorSelectNext :: (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectNext f g lec =
  case S.viewl $ nonEmptyCursorNext lec of
    EmptyL -> Nothing
    e :< rest ->
      Just $
      lec
        { nonEmptyCursorPrev = nonEmptyCursorPrev lec |> f (nonEmptyCursorCurrent lec)
        , nonEmptyCursorCurrent = g e
        , nonEmptyCursorNext = rest
        }

nonEmptyCursorSelectFirst :: (a -> b) -> (b -> a) -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectFirst f g lec =
  case nonEmptyCursorSelectPrev f g lec of
    Nothing -> lec
    Just lec' -> nonEmptyCursorSelectFirst f g lec'

nonEmptyCursorSelectLast :: (a -> b) -> (b -> a) -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectLast f g lec =
  case nonEmptyCursorSelectNext f g lec of
    Nothing -> lec
    Just lec' -> nonEmptyCursorSelectLast f g lec'

nonEmptyCursorSelection :: NonEmptyCursor a b -> Int
nonEmptyCursorSelection = length . nonEmptyCursorPrev

nonEmptyCursorSelectIndex ::
     (a -> b) -> (b -> a) -> Int -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectIndex f g i nec
  | i < nonEmptyCursorSelection nec =
    nonEmptyCursorSelectPrev f g nec >>= nonEmptyCursorSelectIndex f g i
  | i > nonEmptyCursorSelection nec =
    nonEmptyCursorSelectNext f g nec >>= nonEmptyCursorSelectIndex f g i
  | otherwise = Just nec

nonEmptyCursorInsert :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsert c lec = lec {nonEmptyCursorPrev = nonEmptyCursorPrev lec |> c}

nonEmptyCursorAppend :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppend c lec = lec {nonEmptyCursorNext = c <| nonEmptyCursorNext lec}

nonEmptyCursorInsertAndSelect :: (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAndSelect f c lec =
  lec
    { nonEmptyCursorCurrent = c
    , nonEmptyCursorNext = f (nonEmptyCursorCurrent lec) <| nonEmptyCursorNext lec
    }

nonEmptyCursorAppendAndSelect :: (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAndSelect f c lec =
  lec
    { nonEmptyCursorPrev = nonEmptyCursorPrev lec |> f (nonEmptyCursorCurrent lec)
    , nonEmptyCursorCurrent = c
    }

nonEmptyCursorInsertAtStart :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAtStart c lec = lec {nonEmptyCursorPrev = c <| nonEmptyCursorPrev lec}

nonEmptyCursorAppendAtEnd :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAtEnd c lec = lec {nonEmptyCursorNext = nonEmptyCursorNext lec |> c}

nonEmptyCursorInsertAtStartAndSelect ::
     (a -> b) -> (b -> a) -> b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAtStartAndSelect f g c =
  nonEmptyCursorSelectFirst f g . nonEmptyCursorInsertAtStart c

nonEmptyCursorAppendAtEndAndSelect ::
     (a -> b) -> (b -> a) -> b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAtEndAndSelect f g c =
  nonEmptyCursorSelectLast f g . nonEmptyCursorAppendAtEnd c

nonEmptyCursorRemoveElemAndSelectPrev ::
     (b -> a) -> NonEmptyCursor a b -> Maybe (DeleteOrUpdate (NonEmptyCursor a b))
nonEmptyCursorRemoveElemAndSelectPrev g lec =
  case S.viewr $ nonEmptyCursorPrev lec of
    EmptyR ->
      case S.viewl $ nonEmptyCursorNext lec of
        EmptyL -> Just Deleted
        _ -> Nothing
    rest :> e -> Just $ Updated $ lec {nonEmptyCursorPrev = rest, nonEmptyCursorCurrent = g e}

nonEmptyCursorDeleteElemAndSelectNext ::
     (b -> a) -> NonEmptyCursor a b -> Maybe (DeleteOrUpdate (NonEmptyCursor a b))
nonEmptyCursorDeleteElemAndSelectNext g lec =
  case S.viewl $ nonEmptyCursorNext lec of
    EmptyL ->
      case S.viewr $ nonEmptyCursorPrev lec of
        EmptyR -> Just Deleted
        _ -> Nothing
    e :< rest -> Just $ Updated $ lec {nonEmptyCursorCurrent = g e, nonEmptyCursorNext = rest}

nonEmptyCursorRemoveElem :: (b -> a) -> NonEmptyCursor a b -> DeleteOrUpdate (NonEmptyCursor a b)
nonEmptyCursorRemoveElem g lec =
  joinDeletes
    (nonEmptyCursorRemoveElemAndSelectPrev g lec)
    (nonEmptyCursorDeleteElemAndSelectNext g lec)

nonEmptyCursorDeleteElem :: (b -> a) -> NonEmptyCursor a b -> DeleteOrUpdate (NonEmptyCursor a b)
nonEmptyCursorDeleteElem g lec =
  joinDeletes
    (nonEmptyCursorDeleteElemAndSelectNext g lec)
    (nonEmptyCursorRemoveElemAndSelectPrev g lec)

nonEmptyCursorSearch ::
     (a -> b) -> (b -> a) -> (a -> Bool) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSearch f g p nec =
  if p $ nonEmptyCursorCurrent nec
    then Just nec
    else lookPrev nec `mplus` lookNext nec
  where
    lookPrev = look nonEmptyCursorSelectPrev
    lookNext = look nonEmptyCursorSelectNext
    look func nec_ = do
      nec' <- func f g nec_
      if p $ nonEmptyCursorCurrent nec'
        then Just nec'
        else look func nec'

nonEmptyCursorSelectOrAdd ::
     (a -> b) -> (b -> a) -> (a -> Bool) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectOrAdd f g p a nec =
  case nonEmptyCursorSearch f g p nec of
    Nothing -> nonEmptyCursorAppendAndSelect f a nec
    Just nec' -> nec'

nonemptyPrepend :: Seq a -> NonEmpty a -> NonEmpty a
nonemptyPrepend ls ne = foldr NE.cons ne ls

nonemptyAppend :: NonEmpty a -> [a] -> NonEmpty a
nonemptyAppend (x :| xs) ls = x :| (xs ++ ls)

traverseNonEmptyCursor :: (Seq b -> a -> Seq b -> f c) -> NonEmptyCursor a b -> f c
traverseNonEmptyCursor = foldNonEmptyCursor

foldNonEmptyCursor :: (Seq b -> a -> Seq b -> c) -> NonEmptyCursor a b -> c
foldNonEmptyCursor func NonEmptyCursor {..} =
  func nonEmptyCursorPrev nonEmptyCursorCurrent nonEmptyCursorNext
