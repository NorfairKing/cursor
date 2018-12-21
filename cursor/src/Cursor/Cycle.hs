{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Cycle
    ( CycleCursor(..)
    , emptyCycleCursor
    , makeCycleCursor
    , makeCycleCursorWithSelection
    , rebuildCycleCursor
    , cycleCursorToList
    , cycleCursorNull
    , cycleCursorLength
    , cycleCursorIndex
    , cycleCursorSelectPrev
    , cycleCursorSelectNext
    , cycleCursorSelectIndex
    , cycleCursorSelectStart
    , cycleCursorSelectEnd
    , cycleCursorPrevItem
    , cycleCursorNextItem
    , cycleCursorInsert
    , cycleCursorAppend
    , cycleCursorRemove
    , cycleCursorDelete
    , cycleCursorAllRotations
    , cycleCursorCycleTo
    , cycleCursorCycleUntil
    ) where

import GHC.Generics (Generic)

import Data.List
import Data.Maybe
import Data.Validity

import Lens.Micro

import Cursor.List
import Cursor.Types

newtype CycleCursor a = CycleCursor
    { cycleCursorListCursor :: ListCursor a
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (CycleCursor a)

cycleCursorListCursorL :: Lens' (CycleCursor a) (ListCursor a)
cycleCursorListCursorL =
    lens cycleCursorListCursor $ \cc lc -> cc {cycleCursorListCursor = lc}

emptyCycleCursor :: CycleCursor a
emptyCycleCursor = CycleCursor emptyListCursor

makeCycleCursor :: [a] -> CycleCursor a
makeCycleCursor = CycleCursor . makeListCursor

makeCycleCursorWithSelection :: Int -> [a] -> Maybe (CycleCursor a)
makeCycleCursorWithSelection i =
    fmap CycleCursor . makeListCursorWithSelection i

-- | Turn the cycle cursor back into its original list.
rebuildCycleCursor :: CycleCursor a -> [a]
rebuildCycleCursor = rebuildListCursor . cycleCursorListCursor

-- | Make a list of all elements in the cycle cursor, starting from the current selection
cycleCursorToList :: CycleCursor a -> [a]
cycleCursorToList (CycleCursor ListCursor {..}) =
    listCursorNext ++ reverse listCursorPrev

cycleCursorNull :: CycleCursor a -> Bool
cycleCursorNull = listCursorNull . cycleCursorListCursor

cycleCursorLength :: CycleCursor a -> Int
cycleCursorLength = listCursorLength . cycleCursorListCursor

cycleCursorIndex :: CycleCursor a -> Int
cycleCursorIndex = listCursorIndex . cycleCursorListCursor

cycleCursorSelectPrev :: CycleCursor a -> Maybe (CycleCursor a)
cycleCursorSelectPrev cc =
    let ListCursor {..} = cycleCursorListCursor cc
    in case listCursorPrev of
           [] ->
               case listCursorNext of
                   [] -> Nothing
                   _ ->
                       Just $
                       CycleCursor $
                       ListCursor
                       { listCursorPrev = reverse listCursorNext
                       , listCursorNext = []
                       }
           (a:as) ->
               Just $
               CycleCursor
                   ListCursor
                   {listCursorPrev = as, listCursorNext = a : listCursorNext}

cycleCursorSelectNext :: CycleCursor a -> Maybe (CycleCursor a)
cycleCursorSelectNext cc =
    let ListCursor {..} = cycleCursorListCursor cc
    in case listCursorNext of
           [] ->
               case listCursorPrev of
                   [] -> Nothing
                   _ ->
                       Just $
                       CycleCursor $
                       ListCursor
                       { listCursorPrev = []
                       , listCursorNext = reverse listCursorPrev
                       }
           (a:as) ->
               Just $
               CycleCursor
                   ListCursor
                   {listCursorPrev = a : listCursorPrev, listCursorNext = as}

cycleCursorSelectIndex :: Int -> CycleCursor a -> CycleCursor a
cycleCursorSelectIndex ix_ cc@(CycleCursor lc) =
    let ls = rebuildListCursor lc
    in case ls of
           [] -> cc
           _ ->
               case splitAt (ix_ `rem` length ls) ls of
                   (l, r) ->
                       CycleCursor
                           ListCursor
                           {listCursorPrev = reverse l, listCursorNext = r}

cycleCursorSelectStart :: CycleCursor a -> CycleCursor a
cycleCursorSelectStart = cycleCursorListCursorL %~ listCursorSelectStart

cycleCursorSelectEnd :: CycleCursor a -> CycleCursor a
cycleCursorSelectEnd = cycleCursorListCursorL %~ listCursorSelectEnd

cycleCursorPrevItem :: CycleCursor a -> Maybe a
cycleCursorPrevItem cc =
    let ListCursor {..} = cycleCursorListCursor cc
    in case listCursorPrev of
           [] ->
               case reverse listCursorNext of
                   [] -> Nothing
                   (a:_) -> Just a
           (a:_) -> Just a

cycleCursorNextItem :: CycleCursor a -> Maybe a
cycleCursorNextItem cc =
    let ListCursor {..} = cycleCursorListCursor cc
    in case listCursorNext of
           [] ->
               case reverse listCursorPrev of
                   [] -> Nothing
                   (a:_) -> Just a
           (a:_) -> Just a

cycleCursorInsert :: a -> CycleCursor a -> CycleCursor a
cycleCursorInsert a = cycleCursorListCursorL %~ listCursorInsert a

cycleCursorAppend :: a -> CycleCursor a -> CycleCursor a
cycleCursorAppend a = cycleCursorListCursorL %~ listCursorAppend a

cycleCursorRemove :: CycleCursor a -> Maybe (DeleteOrUpdate (CycleCursor a))
cycleCursorRemove =
    focusPossibleDeleteOrUpdate cycleCursorListCursorL listCursorRemove

cycleCursorDelete :: CycleCursor a -> Maybe (DeleteOrUpdate (CycleCursor a))
cycleCursorDelete =
    focusPossibleDeleteOrUpdate cycleCursorListCursorL listCursorDelete

cycleCursorAllRotations :: CycleCursor a -> [CycleCursor a]
cycleCursorAllRotations cc = allPrevRotations ++ [cc] ++ allNextRotations
  where
    allNextRotations = helper listCursorSelectNext
    allPrevRotations = helper listCursorSelectPrev
    helper func = go cc
      where
        go c = do
            case c & cycleCursorListCursorL func of
                Nothing -> []
                Just c' -> c' : go c'

-- | Cycle through the elements until the element right ahead of the selection
-- equals the given element
cycleCursorCycleTo :: Eq a => a -> CycleCursor a -> Maybe (CycleCursor a)
cycleCursorCycleTo a = cycleCursorCycleUntil (== a)

-- | Cycle through the elements until the element right ahead of the selection
-- satisfies the given predicate
cycleCursorCycleUntil :: (a -> Bool) -> CycleCursor a -> Maybe (CycleCursor a)
cycleCursorCycleUntil func cc =
    case (flip mapMaybe (cycleCursorAllRotations cc) $ \c -> do
        e <- cycleCursorNextItem c
        if func e
            then Just c
            else Nothing) of
        [] -> Nothing
        (c:_) -> Just c
