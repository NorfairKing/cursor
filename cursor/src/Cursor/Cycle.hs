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
    ) where

import GHC.Generics (Generic)

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

rebuildCycleCursor :: CycleCursor a -> [a]
rebuildCycleCursor = rebuildListCursor . cycleCursorListCursor

cycleCursorNull :: CycleCursor a -> Bool
cycleCursorNull = listCursorNull . cycleCursorListCursor

cycleCursorLength :: CycleCursor a -> Int
cycleCursorLength = listCursorLength . cycleCursorListCursor

cycleCursorIndex :: CycleCursor a -> Int
cycleCursorIndex = listCursorIndex . cycleCursorListCursor

cycleCursorSelectPrev :: CycleCursor a -> Maybe (CycleCursor a)
cycleCursorSelectPrev = undefined

cycleCursorSelectNext :: CycleCursor a -> Maybe (CycleCursor a)
cycleCursorSelectNext = undefined

cycleCursorSelectIndex :: Int -> CycleCursor a -> CycleCursor a
cycleCursorSelectIndex ix_ = undefined

cycleCursorSelectStart :: CycleCursor a -> CycleCursor a
cycleCursorSelectStart = cycleCursorListCursorL %~ listCursorSelectStart

cycleCursorSelectEnd :: CycleCursor a -> CycleCursor a
cycleCursorSelectEnd = cycleCursorListCursorL %~ listCursorSelectEnd

cycleCursorPrevItem :: CycleCursor a -> a
cycleCursorPrevItem = undefined

cycleCursorNextItem :: CycleCursor a -> a
cycleCursorNextItem = undefined

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
