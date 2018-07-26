{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.List
    ( ListCursor(..)
    , emptyListCursor
    , makeListCursor
    , makeListCursorWithSelection
    , rebuildListCursor
    , listCursorNull
    , listCursorIndex
    , listCursorSelectPrev
    , listCursorSelectNext
    , listCursorSelectIndex
    , listCursorSelectStart
    , listCursorSelectEnd
    , listCursorPrevItem
    , listCursorNextItem
    , listCursorInsert
    , listCursorAppend
    , listCursorBackspace
    , listCursorDelete
    , listCursorSplit
    , listCursorCombine
    ) where

import Data.Validity
import GHC.Generics (Generic)

data ListCursor a = ListCursor
    { listCursorPrev :: [a] -- ^ In reverse order
    , listCursorNext :: [a]
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (ListCursor a)

emptyListCursor :: ListCursor a
emptyListCursor = ListCursor {listCursorPrev = [], listCursorNext = []}

makeListCursor :: [a] -> ListCursor a
makeListCursor = makeListCursorWithSelection 0

makeListCursorWithSelection :: Int -> [a] -> ListCursor a
makeListCursorWithSelection i ls =
    ListCursor
    {listCursorPrev = reverse $ take i ls, listCursorNext = drop i ls}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor ListCursor {..} = reverse listCursorPrev ++ listCursorNext

listCursorNull :: ListCursor a -> Bool
listCursorNull ListCursor {..} = null listCursorPrev && null listCursorNext

listCursorIndex :: ListCursor a -> Int
listCursorIndex = length . listCursorPrev

listCursorSelectPrev :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectPrev tc =
    case listCursorPrev tc of
        [] -> Nothing
        (c:cs) ->
            Just
                ListCursor
                {listCursorPrev = cs, listCursorNext = c : listCursorNext tc}

listCursorSelectNext :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectNext tc =
    case listCursorNext tc of
        [] -> Nothing
        (c:cs) ->
            Just
                ListCursor
                {listCursorPrev = c : listCursorPrev tc, listCursorNext = cs}

listCursorSelectIndex :: Int -> ListCursor a -> ListCursor a
listCursorSelectIndex ix_ lc =
    let ls = rebuildListCursor lc
    in case splitAt ix_ ls of
           (l, r) -> ListCursor {listCursorPrev = reverse l, listCursorNext = r}

listCursorSelectStart :: ListCursor a -> ListCursor a
listCursorSelectStart tc =
    case listCursorSelectPrev tc of
        Nothing -> tc
        Just tc' -> listCursorSelectStart tc'

listCursorSelectEnd :: ListCursor a -> ListCursor a
listCursorSelectEnd tc =
    case listCursorSelectNext tc of
        Nothing -> tc
        Just tc' -> listCursorSelectEnd tc'

listCursorPrevItem :: ListCursor a -> Maybe a
listCursorPrevItem tc =
    case listCursorPrev tc of
        [] -> Nothing
        (c:_) -> Just c

listCursorNextItem :: ListCursor a -> Maybe a
listCursorNextItem tc =
    case listCursorNext tc of
        [] -> Nothing
        (c:_) -> Just c

listCursorInsert :: a -> ListCursor a -> ListCursor a
listCursorInsert c lc = lc {listCursorPrev = c : listCursorPrev lc}

listCursorAppend :: a -> ListCursor a -> ListCursor a
listCursorAppend c lc = lc {listCursorNext = c : listCursorNext lc}

listCursorBackspace :: ListCursor a -> Maybe (ListCursor a)
listCursorBackspace tc =
    case listCursorPrev tc of
        [] -> Nothing
        (_:prev) -> Just $ tc {listCursorPrev = prev}

listCursorDelete :: ListCursor a -> Maybe (ListCursor a)
listCursorDelete tc =
    case listCursorNext tc of
        [] -> Nothing
        (_:next) -> Just $ tc {listCursorNext = next}

listCursorSplit :: ListCursor a -> (ListCursor a, ListCursor a)
listCursorSplit ListCursor {..} =
    ( ListCursor {listCursorPrev = listCursorPrev, listCursorNext = []}
    , ListCursor {listCursorPrev = [], listCursorNext = listCursorNext})

listCursorCombine :: ListCursor a -> ListCursor a -> ListCursor a
listCursorCombine lc1 lc2 =
    ListCursor
    { listCursorPrev = reverse $ rebuildListCursor lc1
    , listCursorNext = rebuildListCursor lc2
    }
