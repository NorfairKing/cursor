{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.List.NonEmpty
  ( NonEmptyCursor (..),
    makeNonEmptyCursor,
    makeNonEmptyCursorWithSelection,
    singletonNonEmptyCursor,
    rebuildNonEmptyCursor,
    nonEmptyCursorElemL,
    mapNonEmptyCursor,
    nonEmptyCursorSelectPrev,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectFirst,
    nonEmptyCursorSelectLast,
    nonEmptyCursorSelection,
    nonEmptyCursorSelectIndex,
    nonEmptyCursorInsert,
    nonEmptyCursorAppend,
    nonEmptyCursorInsertAndSelect,
    nonEmptyCursorAppendAndSelect,
    nonEmptyCursorInsertAtStart,
    nonEmptyCursorAppendAtEnd,
    nonEmptyCursorInsertAtStartAndSelect,
    nonEmptyCursorAppendAtEndAndSelect,
    nonEmptyCursorRemoveElemAndSelectPrev,
    nonEmptyCursorDeleteElemAndSelectNext,
    nonEmptyCursorRemoveElem,
    nonEmptyCursorDeleteElem,
    nonEmptyCursorSearch,
    nonEmptyCursorSelectOrAdd,
    renderNonEmptyCursor,
    nonemptyPrepend,
    nonemptyAppend,
    traverseNonEmptyCursor,
    foldNonEmptyCursor,
  )
where

import Control.DeepSeq
import Control.Monad
import Cursor.Types
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro

-- | A 'nonempty list' cursor
data NonEmptyCursor a b
  = NonEmptyCursor
      { nonEmptyCursorPrev :: [b], -- In reverse order
        nonEmptyCursorCurrent :: a,
        nonEmptyCursorNext :: [b]
      }
  deriving (Show, Eq, Generic, Functor)

instance (Validity a, Validity b) => Validity (NonEmptyCursor a b)

instance (NFData a, NFData b) => NFData (NonEmptyCursor a b)

makeNonEmptyCursor :: (b -> a) -> NonEmpty b -> NonEmptyCursor a b
makeNonEmptyCursor g = fromJust . makeNonEmptyCursorWithSelection g 0

makeNonEmptyCursorWithSelection :: (b -> a) -> Int -> NonEmpty b -> Maybe (NonEmptyCursor a b)
makeNonEmptyCursorWithSelection g i ne = do
  (l, m, r) <- applyNonEmptySelection ne i
  pure
    NonEmptyCursor
      { nonEmptyCursorPrev = reverse l,
        nonEmptyCursorCurrent = g m,
        nonEmptyCursorNext = r
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
  NonEmptyCursor {nonEmptyCursorPrev = [], nonEmptyCursorCurrent = a, nonEmptyCursorNext = []}

rebuildNonEmptyCursor :: (a -> b) -> NonEmptyCursor a b -> NonEmpty b
rebuildNonEmptyCursor f NonEmptyCursor {..} =
  nonemptyPrepend (reverse nonEmptyCursorPrev) $ f nonEmptyCursorCurrent :| nonEmptyCursorNext

mapNonEmptyCursor :: (a -> c) -> (b -> d) -> NonEmptyCursor a b -> NonEmptyCursor c d
mapNonEmptyCursor f g NonEmptyCursor {..} =
  NonEmptyCursor
    { nonEmptyCursorPrev = map g nonEmptyCursorPrev,
      nonEmptyCursorCurrent = f nonEmptyCursorCurrent,
      nonEmptyCursorNext = map g nonEmptyCursorNext
    }

nonEmptyCursorElemL :: Lens (NonEmptyCursor a c) (NonEmptyCursor b c) a b
nonEmptyCursorElemL = lens nonEmptyCursorCurrent $ \lec le -> lec {nonEmptyCursorCurrent = le}

nonEmptyCursorSelectPrev :: (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectPrev f g lec =
  case nonEmptyCursorPrev lec of
    [] -> Nothing
    (e : rest) ->
      Just $
        lec
          { nonEmptyCursorPrev = rest,
            nonEmptyCursorCurrent = g e,
            nonEmptyCursorNext = f (nonEmptyCursorCurrent lec) : nonEmptyCursorNext lec
          }

nonEmptyCursorSelectNext :: (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectNext f g lec =
  case nonEmptyCursorNext lec of
    [] -> Nothing
    (e : rest) ->
      Just $
        lec
          { nonEmptyCursorPrev = f (nonEmptyCursorCurrent lec) : nonEmptyCursorPrev lec,
            nonEmptyCursorCurrent = g e,
            nonEmptyCursorNext = rest
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
nonEmptyCursorInsert c lec = lec {nonEmptyCursorPrev = c : nonEmptyCursorPrev lec}

nonEmptyCursorAppend :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppend c lec = lec {nonEmptyCursorNext = c : nonEmptyCursorNext lec}

nonEmptyCursorInsertAndSelect :: (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAndSelect f c lec =
  lec
    { nonEmptyCursorCurrent = c,
      nonEmptyCursorNext = f (nonEmptyCursorCurrent lec) : nonEmptyCursorNext lec
    }

nonEmptyCursorAppendAndSelect :: (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAndSelect f c lec =
  lec
    { nonEmptyCursorCurrent = c,
      nonEmptyCursorPrev = f (nonEmptyCursorCurrent lec) : nonEmptyCursorPrev lec
    }

nonEmptyCursorInsertAtStart :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAtStart c lec = lec {nonEmptyCursorPrev = nonEmptyCursorPrev lec ++ [c]}

nonEmptyCursorAppendAtEnd :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAtEnd c lec = lec {nonEmptyCursorNext = nonEmptyCursorNext lec ++ [c]}

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
  case nonEmptyCursorPrev lec of
    [] ->
      case nonEmptyCursorNext lec of
        [] -> Just Deleted
        _ -> Nothing
    (e : rest) -> Just $ Updated $ lec {nonEmptyCursorPrev = rest, nonEmptyCursorCurrent = g e}

nonEmptyCursorDeleteElemAndSelectNext ::
  (b -> a) -> NonEmptyCursor a b -> Maybe (DeleteOrUpdate (NonEmptyCursor a b))
nonEmptyCursorDeleteElemAndSelectNext g lec =
  case nonEmptyCursorNext lec of
    [] ->
      case nonEmptyCursorPrev lec of
        [] -> Just Deleted
        _ -> Nothing
    (e : rest) -> Just $ Updated $ lec {nonEmptyCursorCurrent = g e, nonEmptyCursorNext = rest}

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

renderNonEmptyCursor :: ([b] -> a -> [b] -> c) -> NonEmptyCursor a b -> c
renderNonEmptyCursor f NonEmptyCursor {..} =
  f (reverse nonEmptyCursorPrev) nonEmptyCursorCurrent nonEmptyCursorNext

nonemptyPrepend :: [a] -> NonEmpty a -> NonEmpty a
nonemptyPrepend ls ne = foldr (<|) ne ls

nonemptyAppend :: NonEmpty a -> [a] -> NonEmpty a
nonemptyAppend (x :| xs) ls = x :| (xs ++ ls)

traverseNonEmptyCursor :: ([b] -> a -> [b] -> f c) -> NonEmptyCursor a b -> f c
traverseNonEmptyCursor = foldNonEmptyCursor

foldNonEmptyCursor :: ([b] -> a -> [b] -> c) -> NonEmptyCursor a b -> c
foldNonEmptyCursor func NonEmptyCursor {..} =
  func (reverse nonEmptyCursorPrev) nonEmptyCursorCurrent nonEmptyCursorNext
