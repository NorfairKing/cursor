{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.NonEmpty
    ( NonEmptyCursor(..)
    , makeNonEmptyCursor
    , makeNonEmptyCursorWithSelection
    , singletonNonEmptyCursor
    , rebuildNonEmptyCursor
    , nonEmptyCursorSelection
    , nonEmptyCursorElemL
    , nonEmptyCursorSelectPrev
    , nonEmptyCursorSelectNext
    , nonEmptyCursorSelectFirst
    , nonEmptyCursorSelectLast
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

import Control.Applicative
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE

-- | A 'nonempty list' cursor
data NonEmptyCursor a = NonEmptyCursor
    { nonEmptyCursorPrev :: [a] -- In reverse order
    , nonEmptyCursorCurrent :: a
    , nonEmptyCursorNext :: [a]
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (NonEmptyCursor a)

makeNonEmptyCursor :: NonEmpty a -> NonEmptyCursor a
makeNonEmptyCursor = makeNonEmptyCursorWithSelection 0

makeNonEmptyCursorWithSelection :: Int -> NonEmpty a -> NonEmptyCursor a
makeNonEmptyCursorWithSelection i ne =
    let (l, m, r) = applyNonEmptySelection ne i
     in NonEmptyCursor
            { nonEmptyCursorPrev = reverse l
            , nonEmptyCursorCurrent = m
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

singletonNonEmptyCursor :: a -> NonEmptyCursor a
singletonNonEmptyCursor a = makeNonEmptyCursor $ a :| []

rebuildNonEmptyCursor :: NonEmptyCursor a -> NonEmpty a
rebuildNonEmptyCursor NonEmptyCursor {..} =
    nonemptyPrepend (reverse nonEmptyCursorPrev) $
    nonEmptyCursorCurrent :| nonEmptyCursorNext

nonEmptyCursorSelection :: NonEmptyCursor a -> Int
nonEmptyCursorSelection = length . nonEmptyCursorPrev

nonEmptyCursorElemL :: Lens' (NonEmptyCursor a) a
nonEmptyCursorElemL =
    lens nonEmptyCursorCurrent $ \lec le -> lec {nonEmptyCursorCurrent = le}

nonEmptyCursorSelectPrev :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectPrev lec =
    case nonEmptyCursorPrev lec of
        [] -> Nothing
        (e:rest) ->
            Just $
            lec
                { nonEmptyCursorPrev = rest
                , nonEmptyCursorCurrent = e
                , nonEmptyCursorNext =
                      nonEmptyCursorCurrent lec : nonEmptyCursorNext lec
                }

nonEmptyCursorSelectNext :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectNext lec =
    case nonEmptyCursorNext lec of
        [] -> Nothing
        (e:rest) ->
            Just $
            lec
                { nonEmptyCursorPrev =
                      nonEmptyCursorCurrent lec : nonEmptyCursorPrev lec
                , nonEmptyCursorCurrent = e
                , nonEmptyCursorNext = rest
                }

nonEmptyCursorSelectFirst :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorSelectFirst lec =
    case nonEmptyCursorSelectPrev lec of
        Nothing -> lec
        Just lec' -> nonEmptyCursorSelectFirst lec'

nonEmptyCursorSelectLast :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorSelectLast lec =
    case nonEmptyCursorSelectNext lec of
        Nothing -> lec
        Just lec' -> nonEmptyCursorSelectLast lec'

nonEmptyCursorInsert :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsert c lec =
    lec {nonEmptyCursorPrev = c : nonEmptyCursorPrev lec}

nonEmptyCursorAppend :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppend c lec =
    lec {nonEmptyCursorNext = c : nonEmptyCursorNext lec}

nonEmptyCursorInsertAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsertAndSelect c lec =
    lec
        { nonEmptyCursorCurrent = c
        , nonEmptyCursorNext =
              nonEmptyCursorCurrent lec : nonEmptyCursorNext lec
        }

nonEmptyCursorAppendAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppendAndSelect c lec =
    lec
        { nonEmptyCursorCurrent = c
        , nonEmptyCursorPrev =
              nonEmptyCursorCurrent lec : nonEmptyCursorPrev lec
        }

nonEmptyCursorRemoveElemAndSelectPrev ::
       NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorRemoveElemAndSelectPrev lec =
    case nonEmptyCursorPrev lec of
        [] -> Nothing
        (e:rest) ->
            Just $ lec {nonEmptyCursorPrev = rest, nonEmptyCursorCurrent = e}

nonEmptyCursorDeleteElemAndSelectNext ::
       NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorDeleteElemAndSelectNext lec =
    case nonEmptyCursorNext lec of
        [] -> Nothing
        (e:rest) ->
            Just $ lec {nonEmptyCursorCurrent = e, nonEmptyCursorNext = rest}

nonEmptyCursorRemoveElem :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorRemoveElem lec =
    nonEmptyCursorRemoveElemAndSelectPrev lec <|>
    nonEmptyCursorDeleteElemAndSelectNext lec

nonEmptyCursorDeleteElem :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorDeleteElem lec =
    nonEmptyCursorDeleteElemAndSelectNext lec <|>
    nonEmptyCursorRemoveElemAndSelectPrev lec

nonemptyPrepend :: [a] -> NonEmpty a -> NonEmpty a
nonemptyPrepend ls ne = foldr (<|) ne ls

nonemptyAppend :: NonEmpty a -> [a] -> NonEmpty a
nonemptyAppend (x :| xs) ls = x :| (xs ++ ls)
