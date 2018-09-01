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
    , nonEmptyCursorRemoveElemAndSelectPrev
    , nonEmptyCursorDeleteElemAndSelectNext
    , nonEmptyCursorRemoveElem
    , nonEmptyCursorDeleteElem
    , nonemptyPrepend
    , nonemptyAppend
    ) where

import GHC.Generics (Generic)

import Data.Validity

import Lens.Micro

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE

import Cursor.Types

-- | A 'nonempty list' cursor
data NonEmptyCursor a b = NonEmptyCursor
    { nonEmptyCursorPrev :: [b] -- In reverse order
    , nonEmptyCursorCurrent :: a
    , nonEmptyCursorNext :: [b]
    } deriving (Show, Eq, Generic, Functor)

instance (Validity a, Validity b) => Validity (NonEmptyCursor a b)

makeNonEmptyCursor :: (b -> a) -> NonEmpty b -> NonEmptyCursor a b
makeNonEmptyCursor g = makeNonEmptyCursorWithSelection g 0

makeNonEmptyCursorWithSelection ::
       (b -> a) -> Int -> NonEmpty b -> NonEmptyCursor a b
makeNonEmptyCursorWithSelection g i ne =
    let (l, m, r) = applyNonEmptySelection ne i
    in NonEmptyCursor
       { nonEmptyCursorPrev = reverse l
       , nonEmptyCursorCurrent = g m
       , nonEmptyCursorNext = r
       }
  where
    applyNonEmptySelection :: NonEmpty a -> Int -> ([a], a, [a])
    applyNonEmptySelection (c :| rest) i_
        | i_ <= 0 = ([], c, rest)
        | otherwise =
            case NE.nonEmpty rest of
                Nothing -> ([], c, [])
                Just ne_ ->
                    let (l, m, r) = applyNonEmptySelection ne_ (i_ - 1)
                    in (c : l, m, r)

singletonNonEmptyCursor :: a -> NonEmptyCursor a b
singletonNonEmptyCursor a =
    NonEmptyCursor
    { nonEmptyCursorPrev = []
    , nonEmptyCursorCurrent = a
    , nonEmptyCursorNext = []
    }

rebuildNonEmptyCursor :: (a -> b) -> NonEmptyCursor a b -> NonEmpty b
rebuildNonEmptyCursor f NonEmptyCursor {..} =
    nonemptyPrepend (reverse nonEmptyCursorPrev) $
    f nonEmptyCursorCurrent :| nonEmptyCursorNext

mapNonEmptyCursor ::
       (a -> c) -> (b -> d) -> NonEmptyCursor a b -> NonEmptyCursor c d
mapNonEmptyCursor f g NonEmptyCursor {..} =
    NonEmptyCursor
    { nonEmptyCursorPrev = map g nonEmptyCursorPrev
    , nonEmptyCursorCurrent = f nonEmptyCursorCurrent
    , nonEmptyCursorNext = map g nonEmptyCursorNext
    }

nonEmptyCursorElemL :: Lens' (NonEmptyCursor a b) a
nonEmptyCursorElemL =
    lens nonEmptyCursorCurrent $ \lec le -> lec {nonEmptyCursorCurrent = le}

nonEmptyCursorSelectPrev ::
       (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectPrev f g lec =
    case nonEmptyCursorPrev lec of
        [] -> Nothing
        (e:rest) ->
            Just $
            lec
            { nonEmptyCursorPrev = rest
            , nonEmptyCursorCurrent = g e
            , nonEmptyCursorNext =
                  f (nonEmptyCursorCurrent lec) : nonEmptyCursorNext lec
            }

nonEmptyCursorSelectNext ::
       (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectNext f g lec =
    case nonEmptyCursorNext lec of
        [] -> Nothing
        (e:rest) ->
            Just $
            lec
            { nonEmptyCursorPrev =
                  f (nonEmptyCursorCurrent lec) : nonEmptyCursorPrev lec
            , nonEmptyCursorCurrent = g e
            , nonEmptyCursorNext = rest
            }

nonEmptyCursorSelectFirst ::
       (a -> b) -> (b -> a) -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectFirst f g lec =
    case nonEmptyCursorSelectPrev f g lec of
        Nothing -> lec
        Just lec' -> nonEmptyCursorSelectFirst f g lec'

nonEmptyCursorSelectLast ::
       (a -> b) -> (b -> a) -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectLast f g lec =
    case nonEmptyCursorSelectNext f g lec of
        Nothing -> lec
        Just lec' -> nonEmptyCursorSelectLast f g lec'

nonEmptyCursorSelection :: NonEmptyCursor a b -> Int
nonEmptyCursorSelection = length . nonEmptyCursorPrev

nonEmptyCursorSelectIndex ::
       (a -> b)
    -> (b -> a)
    -> Int
    -> NonEmptyCursor a b
    -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectIndex f g i nec
    | i < nonEmptyCursorSelection nec =
        nonEmptyCursorSelectPrev f g nec >>= nonEmptyCursorSelectIndex f g i
    | i > nonEmptyCursorSelection nec =
        nonEmptyCursorSelectNext f g nec >>= nonEmptyCursorSelectIndex f g i
    | otherwise = Just nec

nonEmptyCursorInsert :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsert c lec =
    lec {nonEmptyCursorPrev = c : nonEmptyCursorPrev lec}

nonEmptyCursorAppend :: b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppend c lec =
    lec {nonEmptyCursorNext = c : nonEmptyCursorNext lec}

nonEmptyCursorInsertAndSelect ::
       (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAndSelect f c lec =
    lec
    { nonEmptyCursorCurrent = c
    , nonEmptyCursorNext =
          f (nonEmptyCursorCurrent lec) : nonEmptyCursorNext lec
    }

nonEmptyCursorAppendAndSelect ::
       (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAndSelect f c lec =
    lec
    { nonEmptyCursorCurrent = c
    , nonEmptyCursorPrev =
          f (nonEmptyCursorCurrent lec) : nonEmptyCursorPrev lec
    }

nonEmptyCursorRemoveElemAndSelectPrev ::
       (b -> a)
    -> NonEmptyCursor a b
    -> Maybe (DeleteOrUpdate (NonEmptyCursor a b))
nonEmptyCursorRemoveElemAndSelectPrev g lec =
    case nonEmptyCursorPrev lec of
        [] ->
            case nonEmptyCursorNext lec of
                [] -> Just Deleted
                _ -> Nothing
        (e:rest) ->
            Just $
            Updated $
            lec {nonEmptyCursorPrev = rest, nonEmptyCursorCurrent = g e}

-- the first maybe: whether the operation succeeded
-- the second maybe: whether the list is still nonempty
nonEmptyCursorDeleteElemAndSelectNext ::
       (b -> a)
    -> NonEmptyCursor a b
    -> Maybe (DeleteOrUpdate (NonEmptyCursor a b))
nonEmptyCursorDeleteElemAndSelectNext g lec =
    case nonEmptyCursorNext lec of
        [] ->
            case nonEmptyCursorPrev lec of
                [] -> Just Deleted
                _ -> Nothing
        (e:rest) ->
            Just $
            Updated $
            lec {nonEmptyCursorCurrent = g e, nonEmptyCursorNext = rest}

nonEmptyCursorRemoveElem ::
       (b -> a) -> NonEmptyCursor a b -> DeleteOrUpdate (NonEmptyCursor a b)
nonEmptyCursorRemoveElem g lec =
    joinDeletes
        (nonEmptyCursorRemoveElemAndSelectPrev g lec)
        (nonEmptyCursorDeleteElemAndSelectNext g lec)

nonEmptyCursorDeleteElem ::
       (b -> a) -> NonEmptyCursor a b -> DeleteOrUpdate (NonEmptyCursor a b)
nonEmptyCursorDeleteElem g lec =
    joinDeletes
        (nonEmptyCursorDeleteElemAndSelectNext g lec)
        (nonEmptyCursorRemoveElemAndSelectPrev g lec)

nonemptyPrepend :: [a] -> NonEmpty a -> NonEmpty a
nonemptyPrepend ls ne = foldr (<|) ne ls

nonemptyAppend :: NonEmpty a -> [a] -> NonEmpty a
nonemptyAppend (x :| xs) ls = x :| (xs ++ ls)
